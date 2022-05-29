{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal
  ( pPrintModule
  , pPrintModuleAndCheck
  ) where

import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import Data.Foldable
import Data.HList.HList
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Maybe
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
import GHC.Types.SrcLoc
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
import Language.Haskell.GHC.ExactPrint.Utils (tokComment)

-- BrittanyErrors can be non-fatal warnings, thus both are returned instead
-- of an Either.
-- This should be cleaned up once it is clear what kinds of errors really
-- can occur.
pPrintModule
  :: Config
  -> GHC.ParsedSource
  -> (Text, [BrittanyError], Seq String)
pPrintModule conf (L _ m) =
  (TL.toStrict (TLB.toLazyText out), errs, logs)
  where
    ((out, errs), logs) =
      runIdentity
        $ MultiRWSS.runMultiRWSTNil
        $ MultiRWSS.withMultiWriterAW
        $ MultiRWSS.withMultiWriterAW
        $ MultiRWSS.withMultiWriterW
        $ MultiRWSS.withMultiReader conf
        $ ppModule m

-- | Additionally checks that the output can be parssed again,
-- appending an error if it does not.
pPrintModuleAndCheck
  :: Config
  -> GHC.ParsedSource
  -> IO (Text, [BrittanyError], Seq String)
pPrintModuleAndCheck conf parsedModule = do
  let ghcOptions = runIdentity (_options_ghc (_conf_forward conf))
  let (output, errs, logs) = pPrintModule conf parsedModule
  parseResult <- parseModuleFromString
    ghcOptions
    "output"
    (\_ -> return $ Right ())
    (T.unpack output)
  let errs' = errs ++ case parseResult of
        Left msg -> [ErrorOutputCheck msg]
        Right{}  -> []
  pure (output, errs', logs)

ppModule :: HsModule -> PPM ()
ppModule m@HsModule{hsmodAnn, hsmodDecls} = do
  (config :: CConfig Identity) <- mAsk

  let config' :: CConfig Identity
      config' = config
      exactprintOnly :: Bool
      exactprintOnly = confUnpack (_conf_roundtrip_exactprint_only config')

      (annsBefore, annotatedDecls, annsAfter) = splitAnnots' m

  ppPreamble $ setModComments annsBefore m

  for_ annotatedDecls $ \(cs, decl) -> do
    let addAnn :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
        addAnn = case cs of
          []             -> id
          L anchor _ : _ -> wrapBefore $ EpAnn
            { entry    = anchor
            , anns     = ()
            , comments = EpaComments cs
            }
    layoutBriDoc =<< if exactprintOnly
      then briDocMToPPM $ addAnn $ briDocByExactNoComment decl
      else do
        (r, errs, debugs) <- briDocMToPPMInner $ addAnn $ layoutDecl decl
        mTell debugs
        mTell errs
        if null errs
          then pure r
          else briDocMToPPM $ addAnn $ briDocByExactNoComment decl

  for_ annsAfter $ \comment@(L pos EpaComment{ac_tok}) -> do
    case anchor_op pos of
      UnchangedAnchor -> pure ()
      MovedAnchor dp  -> ppmMoveToExactLoc dp
    case ac_tok of
      EpaEofComment -> pure ()
      _             ->
        mTell $ TLB.fromString $ ExactPrint.commentContents $ tokComment comment

  pure ()

commentLoc :: LEpaComment -> RealSrcSpan
commentLoc = anchor . getLoc

declLoc :: LHsDecl GhcPs -> RealSrcSpan
declLoc = anchor . entry . ann . getLoc

setModComments :: [LEpaComment] -> HsModule -> HsModule
setModComments comments m@HsModule{hsmodAnn} = m
  { hsmodAnn = case hsmodAnn of
    EpAnn entry anns cs -> EpAnn entry anns (cs { priorComments = comments })
    EpAnnNotUsed        -> EpAnnNotUsed
  }

splitAnnots' :: HsModule -> ([LEpaComment], [([LEpaComment], LHsDecl GhcPs)], [LEpaComment])
splitAnnots' m@HsModule{hsmodAnn, hsmodDecls} =
  case hsmodDecls of
    []    -> (allComments, [], [])
    decls -> (commentsBefore, decls', commentsAfter)
      where
        whereLoc :: Maybe RealSrcSpan
        whereLoc =
          case mapMaybe extractWhereLoc $ am_main $ anns hsmodAnn of
            s1 : s2 : _ -> error $
              "Module has more than one 'where' keyword: " ++ show s1 ++ " and " ++ show s2
            []          -> Nothing
            [whereSpan] -> Just whereSpan

        (commentsBefore, rest) = case whereLoc of
          Nothing  -> ([], allComments)
          Just loc -> L.span (\x -> commentLoc x <= loc) allComments

        (decls', commentsAfter) = annotateDecls [] rest decls

        annotateDecls
          :: [([LEpaComment], LHsDecl GhcPs)]
          -> [LEpaComment]
          -> [LHsDecl GhcPs]
          -> ([([LEpaComment], LHsDecl GhcPs)], [LEpaComment])
        annotateDecls acc cs []       = (L.reverse acc, cs)
        annotateDecls acc cs (d : ds) = annotateDecls ((declComments, d) : acc) cs' ds
          where
            (declComments, cs') = L.span (\x -> commentLoc x <= declLoc d) cs
  where
    allComments :: [LEpaComment]
    allComments = priorComments $ epAnnComments hsmodAnn

    extractWhereLoc :: AddEpAnn -> Maybe RealSrcSpan
    extractWhereLoc = \case
      AddEpAnn AnnWhere (EpaSpan loc)  -> Just loc
      AddEpAnn AnnWhere (EpaDelta _ _) -> error "Unexpected EpaDelta"
      _                                -> Nothing

-- | Prints the information associated with the module annotation,
-- including the imports.
ppPreamble
  :: HsModule
  -> PPM ()
ppPreamble m@HsModule{hsmodAnn, hsmodDecls} = do
  config <- mAsk
  let shouldReformatPreamble =
        confUnpack (_lconfig_reformatModulePreamble (_conf_layout config))
  if shouldReformatPreamble
    then
      layoutBriDoc =<< briDocMToPPM (layoutModule m)
    else do
      let emptyModule = m { hsmodDecls = [] }
      processDefault emptyModule
  pure ()

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
        { _lstate_baseYs           = [0]
          -- Important that we dont use Cols here because moveToAnn
          -- stuff of the first node needs to do its thing properly.
        , _lstate_curYOrAddNewline = InsertNewlines 0
        , _lstate_indLevels        = [0]
        , _lstate_indLevelLinger   = 0
        , _lstate_commentCol       = Nothing
        , _lstate_addSepSpace      = Nothing
        , _lstate_commentNewlines  = 0
        }

  _ <- MultiRWSS.withMultiStateS state $ layoutBriDocM briDoc'

  return $ ()
