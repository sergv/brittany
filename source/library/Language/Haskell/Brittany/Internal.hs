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

-- BrittanyErrors can be non-fatal warnings, thus both are returned instead
-- of an Either.
-- This should be cleaned up once it is clear what kinds of errors really
-- can occur.
pPrintModule
  :: Config
  -> GHC.ParsedSource
  -> (Text, [BrittanyError], Seq String)
pPrintModule conf parsedModule =
  (TL.toStrict (TLB.toLazyText out), errs, logs)
  where
    ((out, errs), logs) =
      runIdentity
        $ MultiRWSS.runMultiRWSTNil
        $ MultiRWSS.withMultiWriterAW
        $ MultiRWSS.withMultiWriterAW
        $ MultiRWSS.withMultiWriterW
        $ MultiRWSS.withMultiReader conf
        $ ppModule parsedModule

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

-- toLocal :: Config -> PPMLocal a -> PPM a
-- toLocal conf m = do
--   (x, write) <-
--     lift $ MultiRWSS.runMultiRWSTAW (conf :+: HNil) HNil $ m
--   MultiRWSS.mGetRawW >>= \w -> MultiRWSS.mPutRawW (w `mappend` write)
--   pure x

ppModule :: GenLocated SrcSpan HsModule -> PPM ()
ppModule lmod@(L _ HsModule{hsmodAnn, hsmodDecls}) = do
  ppPreamble lmod
  for_ hsmodDecls $ \decl -> do
    (config :: CConfig Identity) <- mAsk

    let config' :: CConfig Identity
        config' = config
        exactprintOnly :: Bool
        exactprintOnly = confUnpack (_conf_roundtrip_exactprint_only config')

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

-- getDeclBindingNames :: LHsDecl GhcPs -> [String]
-- getDeclBindingNames (L _ decl) = case decl of
--   SigD _ (TypeSig _ ns _) -> ns <&> \(L _ n) -> Text.unpack (rdrNameToText n)
--   ValD _ (FunBind _ (L _ n) _ _) -> [Text.unpack $ rdrNameToText n]
--   _ -> []


-- Prints the information associated with the module annotation
-- This includes the imports
ppPreamble
  :: GenLocated SrcSpan HsModule
  -> PPM ()
ppPreamble lmod@(L loc m@HsModule{hsmodAnn, hsmodDecls}) = do
  --  let filteredAnns = hsmodAnn
  -- filteredAnns <- mAsk <&> \(ToplevelAnns annMap) ->
  --   Map.findWithDefault Map.empty (ExactPrint.mkAnnKey lmod) annMap
  --   -- Since ghc-exactprint adds annotations following (implicit)
  --   -- modules to both HsModule and the elements in the module
  --   -- this can cause duplication of comments. So strip
  --   -- attached annotations that come after the module's where
  --   -- from the module node

  config <- mAsk
  let
    shouldReformatPreamble =
      config & _conf_layout & _lconfig_reformatModulePreamble & confUnpack

  -- let mAnn = hsmodAn
  --     (filteredAnns', post) =
  --       let modAnnsDp = ExactPrint.annsDP mAnn
  --           isWhere (ExactPrint.G AnnWhere) = True
  --           isWhere _ = False
  --           isEof (ExactPrint.AnnEofPos) = True
  --           isEof _ = False
  --           whereInd = List.findIndex (isWhere . fst) modAnnsDp
  --           eofInd = List.findIndex (isEof . fst) modAnnsDp
  --           (pre, post') = case (whereInd, eofInd) of
  --             (Nothing, Nothing) -> ([], modAnnsDp)
  --             (Just i, Nothing) -> List.splitAt (i + 1) modAnnsDp
  --             (Nothing, Just _i) -> ([], modAnnsDp)
  --             (Just i, Just j) -> List.splitAt (min (i + 1) j) modAnnsDp
  --           mAnn' = mAnn { ExactPrint.annsDP = pre }
  --           filteredAnns'' =
  --             Map.insert (ExactPrint.mkAnnKey lmod) mAnn' filteredAnns
  --       in (filteredAnns'', post')

  if shouldReformatPreamble
    then do
      briDoc <- briDocMToPPM $ layoutModule lmod
      layoutBriDoc briDoc
    else
      let emptyModule = L loc m { hsmodDecls = [] }
      in processDefault emptyModule
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
