{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal
  ( pPrintModule
  , pPrintModuleAndCheck
  , getTopLevelDeclNameMap
  ) where

import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import Data.HList.HList
import qualified Data.Map as Map
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import GHC (GenLocated(L))
import qualified GHC hiding (parseModule)
import GHC.Hs
import qualified GHC.OldList as List
import GHC.Parser.Annotation (AnnKeywordId(..))
import GHC.Types.SrcLoc (SrcSpan)
import Language.Haskell.Brittany.Internal.Backend
import Language.Haskell.Brittany.Internal.BackendUtils
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.ExactPrintUtils
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.Decl
import Language.Haskell.Brittany.Internal.Layouters.Module
import Language.Haskell.Brittany.Internal.ParseModule
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Transformations.Alt
import Language.Haskell.Brittany.Internal.Transformations.Columns
import Language.Haskell.Brittany.Internal.Transformations.Floating
import Language.Haskell.Brittany.Internal.Transformations.Indent
import Language.Haskell.Brittany.Internal.Transformations.Par
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint

getTopLevelDeclNameMap :: GHC.ParsedSource -> TopLevelDeclNameMap
getTopLevelDeclNameMap (L _ HsModule{hsmodDecls}) =
  TopLevelDeclNameMap $ Map.fromList
    [ (ExactPrint.mkAnnKey decl, name)
    | decl <- hsmodDecls
    , (name : _) <- [getDeclBindingNames decl]
    ]

-- BrittanyErrors can be non-fatal warnings, thus both are returned instead
-- of an Either.
-- This should be cleaned up once it is clear what kinds of errors really
-- can occur.
pPrintModule
  :: Config
  -> ExactPrint.Anns
  -> GHC.ParsedSource
  -> (Text, [BrittanyError], Seq String)
pPrintModule conf anns parsedModule =
  let
    ((out, errs), logs) =
      runIdentity
        $ MultiRWSS.runMultiRWSTNil
        $ MultiRWSS.withMultiWriterAW
        $ MultiRWSS.withMultiWriterAW
        $ MultiRWSS.withMultiWriterW
        $ MultiRWSS.withMultiReader anns
        $ MultiRWSS.withMultiReader conf
        $ MultiRWSS.withMultiReader (extractToplevelAnns parsedModule anns)
        $ do
            traceIfDumpConf "bridoc annotations raw" _dconf_dump_annotations
              $ annsDoc anns
            ppModule parsedModule
  in (TL.toStrict (TLB.toLazyText out), errs, logs)

-- | Additionally checks that the output can be parssed again,
-- appending an error if it does not.
pPrintModuleAndCheck
  :: Config
  -> ExactPrint.Anns
  -> GHC.ParsedSource
  -> IO (Text, [BrittanyError], Seq String)
pPrintModuleAndCheck conf anns parsedModule = do
  let ghcOptions = runIdentity (_options_ghc (_conf_forward conf))
  let (output, errs, logs) = pPrintModule conf anns parsedModule
  parseResult <- parseModuleFromString
    ghcOptions
    "output"
    (\_ -> return $ Right ())
    (T.unpack output)
  let errs' = errs ++ case parseResult of
        Left msg -> [ErrorOutputCheck msg]
        Right{}  -> []
  pure (output, errs', logs)

toLocal :: Config -> ExactPrint.Anns -> PPMLocal a -> PPM a
toLocal conf anns m = do
  (x, write) <-
    lift $ MultiRWSS.runMultiRWSTAW (conf :+: anns :+: HNil) HNil $ m
  MultiRWSS.mGetRawW >>= \w -> MultiRWSS.mPutRawW (w `mappend` write)
  pure x

ppModule :: GenLocated SrcSpan HsModule -> PPM ()
ppModule lmod@(L _loc _m@HsModule{hsmodDecls}) = do
  (defaultAnns :: Map ExactPrint.AnnKey ExactPrint.Annotation) <- do
    ToplevelAnns anns <- mAsk
    let annKey = ExactPrint.mkAnnKey lmod
    let annMap = Map.findWithDefault Map.empty annKey anns
    let isEof = (== ExactPrint.AnnEofPos)
    let overAnnsDP f a = a { ExactPrint.annsDP = f $ ExactPrint.annsDP a }
    pure $ fmap (overAnnsDP . filter $ isEof . fst) annMap

  (post :: [(ExactPrint.KeywordId, ExactPrint.DeltaPos)]) <- ppPreamble lmod
  hsmodDecls `forM_` \decl -> do
    let declAnnKey = ExactPrint.mkAnnKey decl

    filteredAnns <- mAsk <&> \(ToplevelAnns annMap) ->
      Map.union defaultAnns $ Map.findWithDefault Map.empty declAnnKey annMap

    traceIfDumpConf
        "bridoc annotations filtered/transformed"
        _dconf_dump_annotations
      $ annsDoc filteredAnns

    (config :: CConfig Identity) <- mAsk

    let config' :: CConfig Identity
        config' = config


    let exactprintOnly :: Bool
        exactprintOnly = config' & _conf_roundtrip_exactprint_only & confUnpack
    toLocal config' filteredAnns $ do
      bd <- if exactprintOnly
        then briDocMToPPM $ briDocByExactNoComment decl
        else do
          (r, errs, debugs) <- briDocMToPPMInner $ layoutDecl decl
          mTell debugs
          mTell errs
          if null errs
            then pure r
            else briDocMToPPM $ briDocByExactNoComment decl
      layoutBriDoc bd

  let
    finalComments = filter
      (fst .> \case
        ExactPrint.AnnComment{} -> True
        _ -> False
      )
      post
  post `forM_` \case
    (ExactPrint.AnnComment (ExactPrint.Comment cmStr _ _), l) -> do
      ppmMoveToExactLoc l
      mTell $ TLB.fromString cmStr
    (ExactPrint.AnnEofPos, (ExactPrint.DP (eofZ, eofX))) ->
      let
        folder (acc, _) (kw, ExactPrint.DP (y, x)) = case kw of
          ExactPrint.AnnComment cm | span <- ExactPrint.commentIdentifier cm ->
            ( acc + y + GHC.srcSpanEndLine span - GHC.srcSpanStartLine span
            , x + GHC.srcSpanEndCol span - GHC.srcSpanStartCol span
            )
          _ -> (acc + y, x)
        (cmY, cmX) = foldl' folder (0, 0) finalComments
      in ppmMoveToExactLoc $ ExactPrint.DP (eofZ - cmY, eofX - cmX)
    _ -> return ()

getDeclBindingNames :: LHsDecl GhcPs -> [String]
getDeclBindingNames (L _ decl) = case decl of
  SigD _ (TypeSig _ ns _) -> ns <&> \(L _ n) -> Text.unpack (rdrNameToText n)
  ValD _ (FunBind _ (L _ n) _ _) -> [Text.unpack $ rdrNameToText n]
  _ -> []


-- Prints the information associated with the module annotation
-- This includes the imports
ppPreamble
  :: GenLocated SrcSpan HsModule
  -> PPM [(ExactPrint.KeywordId, ExactPrint.DeltaPos)]
ppPreamble lmod@(L loc m@HsModule{}) = do
  filteredAnns <- mAsk <&> \(ToplevelAnns annMap) ->
    Map.findWithDefault Map.empty (ExactPrint.mkAnnKey lmod) annMap
    -- Since ghc-exactprint adds annotations following (implicit)
    -- modules to both HsModule and the elements in the module
    -- this can cause duplication of comments. So strip
    -- attached annotations that come after the module's where
    -- from the module node
  config <- mAsk
  let
    shouldReformatPreamble =
      config & _conf_layout & _lconfig_reformatModulePreamble & confUnpack

  let
    (filteredAnns', post) =
      case Map.lookup (ExactPrint.mkAnnKey lmod) filteredAnns of
        Nothing -> (filteredAnns, [])
        Just mAnn ->
          let
            modAnnsDp = ExactPrint.annsDP mAnn
            isWhere (ExactPrint.G AnnWhere) = True
            isWhere _ = False
            isEof (ExactPrint.AnnEofPos) = True
            isEof _ = False
            whereInd = List.findIndex (isWhere . fst) modAnnsDp
            eofInd = List.findIndex (isEof . fst) modAnnsDp
            (pre, post') = case (whereInd, eofInd) of
              (Nothing, Nothing) -> ([], modAnnsDp)
              (Just i, Nothing) -> List.splitAt (i + 1) modAnnsDp
              (Nothing, Just _i) -> ([], modAnnsDp)
              (Just i, Just j) -> List.splitAt (min (i + 1) j) modAnnsDp
            mAnn' = mAnn { ExactPrint.annsDP = pre }
            filteredAnns'' =
              Map.insert (ExactPrint.mkAnnKey lmod) mAnn' filteredAnns
          in (filteredAnns'', post')
  traceIfDumpConf
      "bridoc annotations filtered/transformed"
      _dconf_dump_annotations
    $ annsDoc filteredAnns'

  if shouldReformatPreamble
    then toLocal config filteredAnns' $ withTransformedAnns lmod $ do
      briDoc <- briDocMToPPM $ layoutModule lmod
      layoutBriDoc briDoc
    else
      let emptyModule = L loc m { hsmodDecls = [] }
      in MultiRWSS.withMultiReader filteredAnns' $ processDefault emptyModule
  return post

layoutBriDoc :: BriDocNumbered -> PPMLocal ()
layoutBriDoc briDoc = do
  -- first step: transform the briDoc.
  briDoc' <- MultiRWSS.withMultiStateS BDEmpty $ do
    -- Note that briDoc is BriDocNumbered, but state type is BriDoc.
    -- That's why the alt-transform looks a bit special here.
    traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw
      $ briDocToDoc
      $ unwrapBriDocNumbered
      $ briDoc
    -- bridoc transformation: remove alts
    transformAlts briDoc >>= mSet
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf "bridoc post-alt" _dconf_dump_bridoc_simpl_alt
    -- bridoc transformation: float stuff in
    mGet >>= transformSimplifyFloating .> mSet
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf
           "bridoc post-floating"
           _dconf_dump_bridoc_simpl_floating
    -- bridoc transformation: par removal
    mGet >>= transformSimplifyPar .> mSet
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf "bridoc post-par" _dconf_dump_bridoc_simpl_par
    -- bridoc transformation: float stuff in
    mGet >>= transformSimplifyColumns .> mSet
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf "bridoc post-columns" _dconf_dump_bridoc_simpl_columns
    -- bridoc transformation: indent
    mGet >>= transformSimplifyIndent .> mSet
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf "bridoc post-indent" _dconf_dump_bridoc_simpl_indent
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf "bridoc final" _dconf_dump_bridoc_final
    -- -- convert to Simple type
    -- simpl <- mGet <&> transformToSimple
    -- return simpl

  let state = LayoutState
        { _lstate_baseYs = [0]
          -- Important that we dont use Cols here because moveToAnn
          -- stuff of the first node needs to do its thing properly.
        , _lstate_curYOrAddNewline = InsertNewlines 0
        , _lstate_indLevels = [0]
        , _lstate_indLevelLinger = 0
        , _lstate_commentCol = Nothing
        , _lstate_addSepSpace = Nothing
        , _lstate_commentNewlines = 0
        }

  _ <- MultiRWSS.withMultiStateS state $ layoutBriDocM briDoc'

  return $ ()
