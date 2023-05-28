{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Brittany.Internal.Transformations.Alt (transformAlts) where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Monad.Memo qualified as Memo
import Control.Monad.Trans.MultiRWS (MonadMultiReader(..), MonadMultiState(..), MonadMultiWriter(..), mGet)
import Control.Monad.Trans.MultiRWS.Strict qualified as MultiRWSS
import Data.Functor
import Data.Functor.Identity
import Data.HList.ContainsType
import Data.List qualified as L
import Data.List.Extra qualified
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Semigroup
import Data.Sequence (Seq)
import Data.Text qualified as T
import Data.Traversable

import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils

data AltCurPos = AltCurPos
  { _acp_line        :: !Int -- chars in the current line
  , _acp_indent      :: !Int -- current indentation level
  , _acp_indentPrep  :: !Int -- indentChange affecting the next Par
  , _acp_forceMLFlag :: AltLineModeState
  } deriving Show

data AltLineModeState
  = AltLineModeStateNone
  | AltLineModeStateForceML Bool -- true ~ decays on next wrap
  | AltLineModeStateForceSL
  | AltLineModeStateContradiction
  -- i.e. ForceX False -> ForceX True -> None
  deriving (Show)

altLineModeRefresh :: AltLineModeState -> AltLineModeState
altLineModeRefresh = \case
  AltLineModeStateNone          -> AltLineModeStateNone
  AltLineModeStateForceML{}     -> AltLineModeStateForceML False
  AltLineModeStateForceSL       -> AltLineModeStateForceSL
  AltLineModeStateContradiction -> AltLineModeStateContradiction

altLineModeDecay :: AltLineModeState -> AltLineModeState
altLineModeDecay = \case
  AltLineModeStateNone          -> AltLineModeStateNone
  AltLineModeStateForceML False -> AltLineModeStateForceML True
  AltLineModeStateForceML True  -> AltLineModeStateNone
  AltLineModeStateForceSL       -> AltLineModeStateForceSL
  AltLineModeStateContradiction -> AltLineModeStateContradiction

mergeLineMode :: AltCurPos -> AltLineModeState -> AltCurPos
mergeLineMode acp s = case (_acp_forceMLFlag acp, s) of
  (AltLineModeStateContradiction, _) -> acp
  (AltLineModeStateNone, x) -> acp { _acp_forceMLFlag = x }
  (AltLineModeStateForceSL, AltLineModeStateForceSL) -> acp
  (AltLineModeStateForceML{}, AltLineModeStateForceML{}) ->
    acp { _acp_forceMLFlag = s }
  _ -> acp { _acp_forceMLFlag = AltLineModeStateContradiction }


-- removes any BDAlt's from the BriDoc
transformAlts
  :: forall r w s
   . ( Data.HList.ContainsType.ContainsType Config r
     , Data.HList.ContainsType.ContainsType (Seq String) w
     )
  => BriDocNumbered
  -> MultiRWSS.MultiRWS r w s BriDoc
transformAlts
  = MultiRWSS.withMultiStateA (AltCurPos 0 0 0 AltLineModeStateNone)
  . Memo.startEvalMemoT
  . fmap unwrapBriDocNumbered
  . rec
  where
    -- this function is exponential by nature and cannot be improved in any
    -- way i can think of, and i've tried. (stupid StableNames.)
    -- transWrap :: BriDoc -> BriDocNumbered
    -- transWrap brDc = flip StateS.evalState (1::Int)
    --                $ Memo.startEvalMemoT
    --                $ go brDc
    --   where
    --     incGet = StateS.get >>= \i -> StateS.put (i+1) $> i
    --     go :: BriDoc -> Memo.MemoT BriDoc BriDocNumbered (StateS.State Int) BriDocNumbered
    --     go = Memo.memo $ \bdX -> do
    --       i <- lift $ incGet
    --       fmap (\bd' -> (i,bd')) $ case bdX of
    --         BDEmpty           -> pure $ BDEmpty
    --         BDLit t           -> pure $ BDLit t
    --         BDSeq list        -> BDSeq <$> go `mapM` list
    --         BDCols sig list   -> BDCols sig <$> go `mapM` list
    --         BDSeparator       -> pure $ BDSeparator
    --         BDAddBaseY ind bd -> BDAddBaseY ind <$> go bd
    --         BDSetBaseY bd     -> BDSetBaseY <$> go bd
    --         BDSetIndentLevel bd     -> BDSetIndentLevel <$> go bd
    --         BDPar ind line indented -> [ BDPar ind line' indented'
    --                                    | line' <- go line
    --                                    , indented' <- go indented
    --                                    ]
    --         BDAlt alts              -> BDAlt <$> go `mapM` alts -- not that this will happen
    --         BDorceMultiline  bd    -> BDForceMultiline <$> go bd
    --         BDorceSingleline bd    -> BDForceSingleline <$> go bd
    --         BDorwardLineMode bd    -> BDForwardLineMode <$> go bd
    --         BDExternal k ks c t          -> pure $ BDExternal k ks c t
    --         BDAnnotationBefore annKey bd -> BDAnnotationBefore annKey <$> go bd
    --         BDAnnotationPost  annKey bd  -> BDAnnotationAter  annKey <$> go bd
    --         BDLines lines         -> BDLines <$> go `mapM` lines
    --         BDEnsureIndent ind bd -> BDEnsureIndent ind <$> go bd
    --         BDProhibitMTEL bd     -> BDProhibitMTEL <$> go bd

  rec
    :: BriDocNumbered
    -> Memo.MemoT
         Int
         [VerticalSpacing]
         (MultiRWSS.MultiRWS r w (AltCurPos ': s))
         BriDocNumbered
  rec bdX@(brDcId :< brDc) = do
    let reWrap = (brDcId :<)
    -- debugAcp :: AltCurPos <- mGet
    case brDc of
      BDEmpty{}            -> processSpacingSimple bdX $> bdX
      BDLit{}              -> processSpacingSimple bdX $> bdX
      BDSeq list           -> reWrap . BDSeq <$> list `for` rec
      BDCols sig list      -> reWrap . BDCols sig <$> list `for` rec
      BDSeparator          -> processSpacingSimple bdX $> bdX
      BDAddBaseY indent bd -> do
        acp    <- mGet
        indAdd <- fixIndentationForMultiple acp indent
        mSet $ acp { _acp_indentPrep = max (_acp_indentPrep acp) indAdd }
        r      <- rec bd
        acp'   <- mGet
        mSet $ acp' { _acp_indent = _acp_indent acp }
        pure $ case indent of
          BrIndentNone -> r
          BrIndentRegular -> reWrap $ BDAddBaseY (BrIndentSpecial indAdd) r
          BrIndentSpecial i -> reWrap $ BDAddBaseY (BrIndentSpecial i) r
      BDBaseYPushCur bd -> do
        acp <- mGet
        mSet $ acp { _acp_indent = _acp_line acp }
        r <- rec bd
        pure $ reWrap $ BDBaseYPushCur r
      BDBaseYPop bd -> do
        acp <- mGet
        r <- rec bd
        acp' <- mGet
        mSet $ acp' { _acp_indent = _acp_indentPrep acp }
        pure $ reWrap $ BDBaseYPop r
      BDIndentLevelPushCur bd -> do
        reWrap . BDIndentLevelPushCur <$> rec bd
      BDIndentLevelPop bd -> do
        reWrap . BDIndentLevelPop <$> rec bd
      BDPar indent sameLine indented -> do
        indAmount <- confUnpack . _lconfig_indentAmount . _conf_layout <$> mAsk
        let indAdd = case indent of
              BrIndentNone      -> 0
              BrIndentRegular   -> indAmount
              BrIndentSpecial i -> i
        acp <- mGet
        let ind = _acp_indent acp + _acp_indentPrep acp + indAdd
        mSet $ acp { _acp_indent = ind, _acp_indentPrep = 0 }
        sameLine' <- rec sameLine
        mModify $ \acp' -> acp' { _acp_line = ind, _acp_indent = ind }
        indented' <- rec indented
        pure $ reWrap $ BDPar indent sameLine' indented'
      BDAlt [] -> error "empty BDAlt" -- returning BDEmpty instead is a
                                      -- possibility, but i will prefer a
                                      -- fail-early approach; BDEmpty does not
                                      -- make sense semantically for Alt[].
      BDAlt alts -> do
        altChooser <- confUnpack . _lconfig_altChooser . _conf_layout <$> mAsk
        case altChooser of
          AltChooserSimpleQuick ->
            rec $ head alts
          AltChooserShallowBest -> do
            spacings <- traverse getSpacing alts
            acp      <- mGet
            let lineCheck LineModeInvalid = False
                lineCheck (LineModeValid (VerticalSpacing _ p _)) =
                  case _acp_forceMLFlag acp of
                    AltLineModeStateNone          -> True
                    AltLineModeStateForceSL{}     -> p == VerticalSpacingParNone
                    AltLineModeStateForceML{}     -> p /= VerticalSpacingParNone
                    AltLineModeStateContradiction -> False
            lconf <- _conf_layout <$> mAsk
            let
              options = -- trace ("considering options:" ++ show (length alts, acp)) $
                (zip spacings alts
                <&> \(vs, bd) -> -- trace ("spacing=" ++ show vs ++ ",hasSpace=" ++ show (hasSpace lconf acp vs) ++ ",lineCheck=" ++ show (lineCheck vs))
                                 (hasSpace1 lconf acp vs && lineCheck vs, bd)
                )
            rec
              $ fromMaybe (L.last alts)
              $ Data.List.Extra.firstJust
                  (\(_i :: Int, (b, x)) ->
                    [ -- traceShow ("choosing option " ++ show i) $
                      x
                    | b
                    ]
                  )
              $ zip [1 ..] options
          AltChooserBoundedSearch limit -> do
            spacings <- alts `for` getSpacings limit
            acp      <- mGet
            let lineCheck (VerticalSpacing _ p _) = case _acp_forceMLFlag acp of
                  AltLineModeStateNone          -> True
                  AltLineModeStateForceSL{}     -> p == VerticalSpacingParNone
                  AltLineModeStateForceML{}     -> p /= VerticalSpacingParNone
                  AltLineModeStateContradiction -> False
            lconf <- _conf_layout <$> mAsk
            let
              options = -- trace ("considering options:" ++ show (length alts, acp)) $
                (zip spacings alts
                <&> \(vs, bd) -> -- trace ("spacing=" ++ show vs ++ ",hasSpace=" ++ show (hasSpace lconf acp vs) ++ ",lineCheck=" ++ show (lineCheck vs))
                      (any (hasSpace2 lconf acp) vs && any lineCheck vs, bd)
                )
            let
              checkedOptions :: [Maybe (Int, BriDocNumbered)] =
                zip [1 ..] options <&> (\(i, (b, x)) -> [ (i, x) | b ])
            rec
              $ fromMaybe (-- trace ("choosing last") $
                           L.last alts)
              $ Data.List.Extra.firstJust (fmap snd) checkedOptions
      BDForceMultiline bd -> do
        acp  <- mGet
        x    <- do
          mSet $ mergeLineMode acp (AltLineModeStateForceML False)
          rec bd
        acp' <- mGet
        mSet $ acp' { _acp_forceMLFlag = _acp_forceMLFlag acp }
        pure $ x
      BDForceSingleline bd -> do
        acp  <- mGet
        x    <- do
          mSet $ mergeLineMode acp AltLineModeStateForceSL
          rec bd
        acp' <- mGet
        mSet $ acp' { _acp_forceMLFlag = _acp_forceMLFlag acp }
        pure $ x
      BDForwardLineMode bd -> do
        acp  <- mGet
        x    <- do
          mSet $ acp
            { _acp_forceMLFlag = altLineModeRefresh $ _acp_forceMLFlag acp
            }
          rec bd
        acp' <- mGet
        mSet $ acp' { _acp_forceMLFlag = _acp_forceMLFlag acp }
        pure $ x
      BDExternal{} -> processSpacingSimple bdX $> bdX
      BDPlain{}    -> processSpacingSimple bdX $> bdX
      BDAnnotationBefore finalDelta comments bd -> do
        acp <- mGet
        mSet $ acp { _acp_forceMLFlag = altLineModeDecay $ _acp_forceMLFlag acp }
        bd' <- rec bd
        pure $ reWrap $ BDAnnotationBefore finalDelta comments bd'
      BDAnnotationAfter comments bd ->
        reWrap . BDAnnotationAfter comments <$> rec bd
      BDAnnotationKW kw bd ->
        reWrap . BDAnnotationKW kw <$> rec bd
      BDMoveToKWDP kw b bd ->
        reWrap . BDMoveToKWDP kw b <$> rec bd
      BDLines [] -> pure $ reWrap BDEmpty -- evil transformation. or harmless.
      BDLines (l : lr) -> do
        ind <- _acp_indent <$> mGet
        l'  <- rec l
        lr' <- lr `for` \x -> do
          mModify $ \acp -> acp { _acp_line = ind, _acp_indent = ind }
          rec x
        pure $ reWrap $ BDLines (l' : lr')
      BDEnsureIndent indent bd -> do
        acp    <- mGet
        indAdd <- fixIndentationForMultiple acp indent
        mSet $ acp
          { _acp_indentPrep = 0
            -- TODO: i am not sure this is valid, in general.
          , _acp_indent = _acp_indent acp + indAdd
          , _acp_line = max (_acp_line acp) (_acp_indent acp + indAdd)
            -- we cannot use just _acp_line acp + indAdd because of the case
            -- where there are multiple BDEnsureIndents in the same line.
            -- Then, the actual indentation is relative to the current
            -- indentation, not the current cursor position.
          }
        r    <- rec bd
        acp' <- mGet
        mSet $ acp' { _acp_indent = _acp_indent acp }
        pure $ case indent of
          BrIndentNone      -> r
          BrIndentRegular   ->
            reWrap $ BDEnsureIndent (BrIndentSpecial indAdd) r
          BrIndentSpecial i -> reWrap $ BDEnsureIndent (BrIndentSpecial i) r
      BDNonBottomSpacing _ bd -> rec bd
      BDSetParSpacing bd      -> rec bd
      BDForceParSpacing bd    -> rec bd
      BDDebug s bd            -> do
        acp :: AltCurPos <- mGet
        tellDebugMess
          $ "transformAlts: BDDEBUG "
          ++ s
          ++ " (node-id="
          ++ show brDcId
          ++ "): acp="
          ++ show acp
        reWrap . BDDebug s <$> rec bd
  processSpacingSimple
    :: ( MonadMultiReader Config m
       , MonadMultiState AltCurPos m
       , MonadMultiWriter (Seq String) m
       )
    => BriDocNumbered
    -> m ()
  processSpacingSimple bd = getSpacing bd >>= \case
    LineModeInvalid -> error "processSpacingSimple inv"
    LineModeValid (VerticalSpacing i VerticalSpacingParNone _) -> do
      acp <- mGet
      mSet $ acp { _acp_line = _acp_line acp + i }
    LineModeValid VerticalSpacing{} -> error "processSpacingSimple par"
  hasSpace1
    :: LayoutConfig -> AltCurPos -> LineModeValidity VerticalSpacing -> Bool
  hasSpace1 _     _   LineModeInvalid    = False
  hasSpace1 lconf acp (LineModeValid vs) = hasSpace2 lconf acp vs

  hasSpace2 :: LayoutConfig -> AltCurPos -> VerticalSpacing -> Bool
  hasSpace2 lconf (AltCurPos line indent indentPrep _) (VerticalSpacing sameLine p _) = case p of
    VerticalSpacingParNone     ->
      line + sameLine <= confUnpack (_lconfig_cols lconf)
    VerticalSpacingParSome par ->
      line + sameLine <= confUnpack (_lconfig_cols lconf) && indent + indentPrep + par <= confUnpack (_lconfig_cols lconf)
    VerticalSpacingParAlways{} ->
      line + sameLine <= confUnpack (_lconfig_cols lconf)

getSpacing
  :: forall m
   . (MonadMultiReader Config m, MonadMultiWriter (Seq String) m)
  => BriDocNumbered
  -> m (LineModeValidity VerticalSpacing)
getSpacing !bridoc = rec bridoc
  where
    rec :: BriDocNumbered -> m (LineModeValidity VerticalSpacing)
    rec (brDcId :< brDc) = do
      config <- mAsk
      let colMax :: Int
          !colMax = confUnpack $ _lconfig_cols $ _conf_layout config
      result <- case brDc of
        BDEmpty ->
          pure $ LineModeValid $ VerticalSpacing 0 VerticalSpacingParNone False
        BDLit t -> pure $ LineModeValid $ VerticalSpacing
          (T.length t)
          VerticalSpacingParNone
          False
        BDSeq list       -> sumVs <$> traverse rec list
        BDCols _sig list -> sumVs <$> traverse rec list
        BDSeparator      ->
          pure $ LineModeValid $ VerticalSpacing 1 VerticalSpacingParNone False
        BDAddBaseY indent bd -> do
          mVs <- rec bd
          pure $ mVs <&> \vs -> vs
            { _vs_paragraph = case _vs_paragraph vs of
              VerticalSpacingParNone     -> VerticalSpacingParNone
              VerticalSpacingParAlways i ->
                VerticalSpacingParAlways $ case indent of
                  BrIndentNone      -> i
                  BrIndentRegular   ->
                    i + confUnpack (_lconfig_indentAmount (_conf_layout config))
                  BrIndentSpecial j -> i + j
              VerticalSpacingParSome i -> VerticalSpacingParSome $ case indent of
                BrIndentNone      -> i
                BrIndentRegular   ->
                  i + confUnpack (_lconfig_indentAmount (_conf_layout config))
                BrIndentSpecial j -> i + j
            }
        BDBaseYPushCur bd -> do
          mVs <- rec bd
          pure $ mVs <&> \vs -> vs
            -- We leave par as-is, even though it technically is not
            -- accurate (in general).
            -- the reason is that we really want to _keep_ it Just if it is
            -- just so we properly communicate the is-multiline fact.
            -- An alternative would be setting to (Just 0).
            { _vs_sameLine = max
              (_vs_sameLine vs)
              (case _vs_paragraph vs of
                VerticalSpacingParNone -> 0
                VerticalSpacingParSome i -> i
                VerticalSpacingParAlways i -> min colMax i
              )
            , _vs_paragraph = VerticalSpacingParSome 0
            }
        BDBaseYPop bd                        -> rec bd
        BDIndentLevelPushCur bd              -> rec bd
        BDIndentLevelPop bd                  -> rec bd
        BDPar BrIndentNone sameLine indented -> do
          mVs    <- rec sameLine
          mIndSp <- rec indented
          pure
            [ VerticalSpacing lsp pspResult parFlagResult
            | VerticalSpacing lsp mPsp _ <- mVs
            , indSp <- mIndSp
            , lineMax <- getMaxVS $ mIndSp
            , let pspResult = case mPsp of
                    VerticalSpacingParSome psp   ->
                      VerticalSpacingParSome $ max psp lineMax
                    VerticalSpacingParNone       ->
                      VerticalSpacingParSome $ lineMax
                    VerticalSpacingParAlways psp ->
                      VerticalSpacingParAlways $ max psp lineMax
            , let parFlagResult
                    =  mPsp == VerticalSpacingParNone
                    && _vs_paragraph indSp == VerticalSpacingParNone
                    && _vs_parFlag indSp
            ]
        BDPar{} -> error "BDPar with indent in getSpacing"
        BDAlt [] -> error "empty BDAlt"
        BDAlt (alt : _) -> rec alt
        BDForceMultiline bd -> do
          mVs <- rec bd
          pure $ do
            x <- mVs
            case _vs_paragraph x of
              VerticalSpacingParNone -> LineModeInvalid
              _                      -> mVs
        BDForceSingleline bd -> do
          mVs <- rec bd
          pure $ do
            x <- mVs
            case _vs_paragraph x of
              VerticalSpacingParNone -> mVs
              _                      -> LineModeInvalid
        BDForwardLineMode bd -> rec bd
        BDExternal _ txt     -> pure $ LineModeValid $ case T.lines txt of
          [t] -> VerticalSpacing (T.length t) VerticalSpacingParNone False
          _   -> VerticalSpacing 999 VerticalSpacingParNone False
        BDPlain txt -> pure $ LineModeValid $ case T.lines txt of
          [t] -> VerticalSpacing (T.length t) VerticalSpacingParNone False
          _   -> VerticalSpacing 999 VerticalSpacingParNone False
        BDAnnotationBefore _ _ bd -> rec bd
        BDAnnotationKW _kw bd     -> rec bd
        BDAnnotationAfter _ bd    -> rec bd
        BDMoveToKWDP _kw _b bd    -> rec bd
        BDLines []                ->
          pure $ LineModeValid $ VerticalSpacing 0 VerticalSpacingParNone False
        BDLines (l : ls) -> do
          lSps <- (:|) <$> rec l <*> traverse rec ls
          let mVs = NE.head lSps
          pure
            [ VerticalSpacing lsp (VerticalSpacingParSome $ lineMax) False
            | VerticalSpacing lsp _ _ <- mVs
            , lineMax <- getMaxVS $ maxVs $ NE.toList lSps
            ]
        BDEnsureIndent indent bd -> do
          mVs <- rec bd
          let addInd = case indent of
                BrIndentNone      -> 0
                BrIndentRegular   -> confUnpack $ _lconfig_indentAmount $ _conf_layout $ config
                BrIndentSpecial i -> i
          pure $ mVs <&> \(VerticalSpacing lsp psp pf) ->
            VerticalSpacing (lsp + addInd) psp pf
        BDNonBottomSpacing b bd -> do
          mVs <- rec bd
          pure $ mVs <|> LineModeValid
            (VerticalSpacing
              0
              (if b
                then VerticalSpacingParSome 0
                else VerticalSpacingParAlways colMax)
              False)
        BDSetParSpacing bd -> do
          mVs <- rec bd
          pure $ mVs <&> \vs -> vs { _vs_parFlag = True }
        BDForceParSpacing bd -> do
          mVs <- rec bd
          pure
            [ vs
            | vs <- mVs
            , _vs_parFlag vs || _vs_paragraph vs == VerticalSpacingParNone
            ]
        BDDebug s bd -> do
          r <- rec bd
          tellDebugMess
            $ "getSpacing: BDDebug "
            ++ show s
            ++ " (node-id="
            ++ show brDcId
            ++ "): mVs="
            ++ show r
          pure r
      pure result
    maxVs
      :: [LineModeValidity VerticalSpacing] -> LineModeValidity VerticalSpacing
    maxVs = L.foldl'
      (liftA2
        (\(VerticalSpacing x1 x2 _) (VerticalSpacing y1 y2 _) -> VerticalSpacing
          (max x1 y1)
          (case (x2, y2) of
            (x, VerticalSpacingParNone) -> x
            (VerticalSpacingParNone, x) -> x
            (VerticalSpacingParAlways i, VerticalSpacingParAlways j) ->
              VerticalSpacingParAlways $ max i j
            (VerticalSpacingParAlways i, VerticalSpacingParSome j) ->
              VerticalSpacingParAlways $ max i j
            (VerticalSpacingParSome j, VerticalSpacingParAlways i) ->
              VerticalSpacingParAlways $ max i j
            (VerticalSpacingParSome x, VerticalSpacingParSome y) ->
              VerticalSpacingParSome $ max x y)
          False))
      (LineModeValid $ VerticalSpacing 0 VerticalSpacingParNone False)
    sumVs
      :: [LineModeValidity VerticalSpacing] -> LineModeValidity VerticalSpacing
    sumVs sps = L.foldl' (liftA2 go) initial sps
      where
        go (VerticalSpacing x1 x2 x3) (VerticalSpacing y1 y2 _) = VerticalSpacing
          (x1 + y1)
          (case (x2, y2) of
            (x, VerticalSpacingParNone) -> x
            (VerticalSpacingParNone, x) -> x
            (VerticalSpacingParAlways i, VerticalSpacingParAlways j) ->
              VerticalSpacingParAlways $ i + j
            (VerticalSpacingParAlways i, VerticalSpacingParSome j) ->
              VerticalSpacingParAlways $ i + j
            (VerticalSpacingParSome i, VerticalSpacingParAlways j) ->
              VerticalSpacingParAlways $ i + j
            (VerticalSpacingParSome x, VerticalSpacingParSome y) ->
              VerticalSpacingParSome $ x + y)
          x3
        singleline (LineModeValid x) = _vs_paragraph x == VerticalSpacingParNone
        singleline _ = False
        isPar (LineModeValid x) = _vs_parFlag x
        isPar _ = False
        parFlag = case sps of
          [] -> True
          _  -> all singleline (L.init sps) && isPar (L.last sps)
        initial = LineModeValid $ VerticalSpacing 0 VerticalSpacingParNone parFlag
    getMaxVS :: LineModeValidity VerticalSpacing -> LineModeValidity Int
    getMaxVS = fmap $ \(VerticalSpacing x1 x2 _) -> x1 `max` case x2 of
      VerticalSpacingParSome i   -> i
      VerticalSpacingParNone     -> 0
      VerticalSpacingParAlways i -> i

data SpecialCompare = Unequal | Smaller | Bigger

getSpacings
  :: forall m
   . (MonadMultiReader Config m, MonadMultiWriter (Seq String) m)
  => Int
  -> BriDocNumbered
  -> Memo.MemoT Int [VerticalSpacing] m [VerticalSpacing]
getSpacings !limit bridoc = preFilterLimit <$> rec bridoc
  where
      -- when we do `take K . filter someCondition` on a list of spacings, we
      -- need to first (also) limit the size of the input list, otherwise a
      -- _large_ input with a similarly _large_ prefix not passing our filtering
      -- process could lead to exponential runtime behaviour.
      -- TODO: 3 is arbitrary.
    preFilterLimit :: [VerticalSpacing] -> [VerticalSpacing]
    preFilterLimit = take (3 * limit)
    memoWithKey :: Memo.MonadMemo k v n => k -> n v -> n v
    memoWithKey k v = Memo.memo (const v) k
    rec :: BriDocNumbered -> Memo.MemoT Int [VerticalSpacing] m [VerticalSpacing]
    rec (brDcId :< brdc) = memoWithKey brDcId $ do
      config <- mAsk
      let colMax :: Int
          !colMax = confUnpack $ _lconfig_cols $ _conf_layout config
      let hasOkColCount (VerticalSpacing lsp psp _) = lsp <= colMax && case psp of
            VerticalSpacingParNone     -> True
            VerticalSpacingParSome i   -> i <= colMax
            VerticalSpacingParAlways{} -> True
      let specialCompare vs1 vs2 =
            if _vs_sameLine vs1 == _vs_sameLine vs2 && _vs_parFlag vs1 == _vs_parFlag vs2
            then case (_vs_paragraph vs1, _vs_paragraph vs2) of
              (VerticalSpacingParAlways i1, VerticalSpacingParAlways i2) ->
                if i1 < i2 then Smaller else Bigger
              (p1, p2) -> if p1 == p2 then Smaller else Unequal
            else Unequal
      let allowHangingQuasiQuotes :: Bool
          allowHangingQuasiQuotes
            = confUnpack
            $ _lconfig_allowHangingQuasiQuotes
            $ _conf_layout config
      let -- this is like L.nub, with one difference: if two elements
          -- are unequal only in _vs_paragraph, with both ParAlways, we
          -- treat them like equals and replace the first occurence with the
          -- smallest member of this "equal group".
          specialNub :: [VerticalSpacing] -> [VerticalSpacing]
          specialNub [] = []
          specialNub (x1 : xr) = case go x1 xr of
            (r, xs') -> r : specialNub xs'
            where
              go y1 [] = (y1, [])
              go y1 (y2 : yr) = case specialCompare y1 y2 of
                Unequal -> let (r, yr') = go y1 yr in (r, y2 : yr')
                Smaller -> go y1 yr
                Bigger  -> go y2 yr
      let -- the standard function used to enforce a constant upper bound
          -- on the number of elements returned for each node. Should be
          -- applied whenever in a parent the combination of spacings from
          -- its children might cause excess of the upper bound.
          filterAndLimit :: [VerticalSpacing] -> [VerticalSpacing]
          filterAndLimit
            = take limit
                         -- prune so we always consider a constant
                         -- amount of spacings per node of the BriDoc.
            . specialNub
                         -- In the end we want to know if there is at least
                         -- one valid spacing for any alternative.
                         -- If there are duplicates in the list, then these
                         -- will either all be valid (so having more than the
                         -- first is pointless) or all invalid (in which
                         -- case having any of them is pointless).
                         -- Nonetheless I think the order of spacings should
                         -- be preserved as it provides a deterministic
                         -- choice for which spacings to prune (which is
                         -- an argument against simply using a Set).
                         -- I have also considered `fmap head . group` which
                         -- seems to work similarly well for common cases
                         -- and which might behave even better when it comes
                         -- to determinism of the algorithm. But determinism
                         -- should not be overrated here either - in the end
                         -- this is about deterministic behaviour of the
                         -- pruning we do that potentially results in
                         -- non-optimal layouts, and we'd rather take optimal
                         -- layouts when we can than take non-optimal layouts
                         -- just to be consistent with other cases where
                         -- we'd choose non-optimal layouts.
            . filter hasOkColCount
                         -- throw out any spacings (i.e. children) that
                         -- already use more columns than available in
                         -- total.
            . preFilterLimit
      result <- case brdc of
        BDEmpty              -> pure $ [VerticalSpacing 0 VerticalSpacingParNone False]
        BDLit t              ->
          pure $ [VerticalSpacing (T.length t) VerticalSpacingParNone False]
        BDSeq list           -> fmap sumVs . traverse filterAndLimit <$> traverse rec list
        BDCols _sig list     -> fmap sumVs . traverse filterAndLimit <$> traverse rec list
        BDSeparator          -> pure $ [VerticalSpacing 1 VerticalSpacingParNone False]
        BDAddBaseY indent bd -> do
          mVs <- rec bd
          pure $ mVs <&> \vs -> vs
            { _vs_paragraph = case _vs_paragraph vs of
              VerticalSpacingParNone     -> VerticalSpacingParNone
              VerticalSpacingParAlways i ->
                VerticalSpacingParAlways $ case indent of
                  BrIndentNone      -> i
                  BrIndentRegular   ->
                    i + confUnpack (_lconfig_indentAmount (_conf_layout config))
                  BrIndentSpecial j -> i + j
              VerticalSpacingParSome i   -> VerticalSpacingParSome $ case indent of
                BrIndentNone      -> i
                BrIndentRegular   ->
                  i + confUnpack (_lconfig_indentAmount (_conf_layout config))
                BrIndentSpecial j -> i + j
            }
        BDBaseYPushCur bd -> do
          mVs <- rec bd
          pure $ mVs <&> \vs -> vs
            -- We leave par as-is, even though it technically is not
            -- accurate (in general).
            -- the reason is that we really want to _keep_ it Just if it is
            -- just so we properly communicate the is-multiline fact.
            -- An alternative would be setting to (Just 0).
            { _vs_sameLine = max
              (_vs_sameLine vs)
              (case _vs_paragraph vs of
                VerticalSpacingParNone     -> 0
                VerticalSpacingParSome i   -> i
                VerticalSpacingParAlways i -> min colMax i)
            , _vs_paragraph = case _vs_paragraph vs of
              VerticalSpacingParNone     -> VerticalSpacingParNone
              VerticalSpacingParSome i   -> VerticalSpacingParSome i
              VerticalSpacingParAlways i -> VerticalSpacingParAlways i
            }
        BDBaseYPop bd                        -> rec bd
        BDIndentLevelPushCur bd              -> rec bd
        BDIndentLevelPop bd                  -> rec bd
        BDPar BrIndentNone sameLine indented -> do
          mVss   <- filterAndLimit <$> rec sameLine
          indSps <- filterAndLimit <$> rec indented
          let mVsIndSp = take limit $ [ (x, y) | x <- mVss, y <- indSps ]
          pure $ mVsIndSp <&> \(VerticalSpacing lsp mPsp _, indSp) ->
            VerticalSpacing
              lsp
              (case mPsp of
                VerticalSpacingParSome psp   ->
                  VerticalSpacingParSome $ max psp $ getMaxVS indSp -- TODO
                VerticalSpacingParNone       ->
                  spMakePar indSp
                VerticalSpacingParAlways psp ->
                  VerticalSpacingParAlways $ max psp $ getMaxVS indSp)
              (mPsp == VerticalSpacingParNone && _vs_paragraph indSp == VerticalSpacingParNone && _vs_parFlag indSp)

        BDPar{}  -> error "BDPar with indent in getSpacing"
        BDAlt [] -> error "empty BDAlt"
        -- BDAlt (alt:_) -> rec alt
        BDAlt alts -> do
          r <- traverse rec alts
          pure $ filterAndLimit =<< r
        BDForceMultiline bd -> do
          mVs <- filterAndLimit <$> rec bd
          pure $ filter ((/= VerticalSpacingParNone) . _vs_paragraph) mVs
        BDForceSingleline bd -> do
          mVs <- filterAndLimit <$> rec bd
          pure $ filter ((== VerticalSpacingParNone) . _vs_paragraph) mVs
        BDForwardLineMode bd -> rec bd
        BDExternal _ txt | [t] <- T.lines txt ->
          pure $ [VerticalSpacing (T.length t) VerticalSpacingParNone False]
        BDExternal{} -> pure $ [] -- yes, we just assume that we cannot properly layout this.
        BDPlain t    -> pure
          [ case T.lines t of
              [] -> VerticalSpacing 0 VerticalSpacingParNone False
              [t1] ->
                VerticalSpacing (T.length t1) VerticalSpacingParNone False
              (t1 : _) ->
                VerticalSpacing (T.length t1) (VerticalSpacingParAlways 0) True
          | allowHangingQuasiQuotes
          ]
        BDAnnotationBefore _ _ bd -> rec bd
        BDAnnotationKW _kw bd     -> rec bd
        BDAnnotationAfter _ bd    -> rec bd
        BDMoveToKWDP _kw _b bd    -> rec bd
        BDLines []                -> pure [VerticalSpacing 0 VerticalSpacingParNone False]
        BDLines ls@(_ : _)        -> do
          -- we simply assume that lines is only used "properly", i.e. in
          -- such a way that the first line can be treated "as a part of the
          -- paragraph". That most importantly means that Lines should never
          -- be inserted anywhere but at the start of the line. A
          -- counterexample would be anything like Seq[Lit "foo", Lines].
          lSpss <- map filterAndLimit <$> traverse rec ls
          let worbled = fmap reverse $ sequence $ reverse $ lSpss
              sumF lSps@(lSp1 : _) =
                VerticalSpacing (_vs_sameLine lSp1) (spMakePar $ maxVs lSps) False
              sumF [] =
                error
                  $ "should not happen. if my logic does not fail"
                  ++ "me, this follows from not (null ls)."
          pure $ sumF <$> worbled
          -- lSpss@(mVs:_) <- rec `mapM` ls
          -- pure $ case Control.Lens.transposeOf traverse lSpss of -- TODO: we currently only
          --                      -- consider the first alternative for the
          --                      -- line's spacings.
          --                      -- also i am not sure if always including
          --                      -- the first line length in the paragraph
          --                      -- length gives the desired results.
          --                      -- it is the safe path though, for now.
          --   []       -> []
          --   (lSps:_) -> mVs <&> \(VerticalSpacing lsp _) ->
          --     VerticalSpacing lsp $ VerticalSpacingParSome $ getMaxVS $ maxVs lSps
        BDEnsureIndent indent bd -> do
          mVs <- rec bd
          let addInd = case indent of
                BrIndentNone      -> 0
                BrIndentRegular   ->
                  confUnpack $ _lconfig_indentAmount $ _conf_layout $ config
                BrIndentSpecial i -> i
          pure $ mVs <&> \(VerticalSpacing lsp psp parFlag) ->
            VerticalSpacing (lsp + addInd) psp parFlag
        BDNonBottomSpacing b bd -> do
          -- TODO: the `b` flag is an ugly hack, but I was not able to make
          -- all tests work without it. It should be possible to have
          -- `spMakePar` map VSPAlways{} to VSPSome x1, which fixes this
          -- problem but breaks certain other cases.
          mVs <- rec bd
          pure $ if null mVs
            then
              [ VerticalSpacing
                  0
                  (if b
                    then VerticalSpacingParSome 0
                    else VerticalSpacingParAlways colMax
                  )
                  False
              ]
            else mVs <&> \vs -> vs
              { _vs_sameLine = min colMax (_vs_sameLine vs)
              , _vs_paragraph = case _vs_paragraph vs of
                VerticalSpacingParNone -> VerticalSpacingParNone
                VerticalSpacingParAlways i
                  | b -> VerticalSpacingParSome 0
                  | otherwise -> VerticalSpacingParAlways i
                VerticalSpacingParSome i
                  | b -> VerticalSpacingParSome 0
                  | otherwise -> VerticalSpacingParAlways i
              }
            -- the version below is an alternative idea: fold the input
            -- spacings into a single spacing. This was hoped to improve in
            -- certain cases where non-bottom alternatives took up "too much
            -- explored search space"; the downside is that it also cuts
            -- the search-space short in other cases where it is not necessary,
            -- leading to unnecessary new-lines. Disabled for now. A better
            -- solution would require conditionally folding the search-space
            -- only in appropriate locations (i.e. a new BriDoc node type
            -- for this purpose, perhaps "BDNonBottomSpacing1").
            -- else
            --   [ Foldable.foldl1
            --     (\(VerticalSpacing x1 x2 _) (VerticalSpacing y1 y2 _) ->
            --       VerticalSpacing
            --         (min x1 y1)
            --         (case (x2, y2) of
            --           (x, VerticalSpacingParNone) -> x
            --           (VerticalSpacingParNone, x) -> x
            --           (VerticalSpacingParAlways i, VerticalSpacingParAlways j) ->
            --             VerticalSpacingParAlways $ min i j
            --           (VerticalSpacingParAlways i, VerticalSpacingParSome j) ->
            --             VerticalSpacingParAlways $ min i j
            --           (VerticalSpacingParSome i, VerticalSpacingParAlways j) ->
            --             VerticalSpacingParAlways $ min i j
            --           (VerticalSpacingParSome x, VerticalSpacingParSome y) ->
            --             VerticalSpacingParSome $ min x y)
            --         False)
            --     mVs
            --   ]
        BDSetParSpacing bd -> do
          mVs <- rec bd
          pure $ mVs <&> \vs -> vs { _vs_parFlag = True }
        BDForceParSpacing bd -> do
          mVs <- preFilterLimit <$> rec bd
          pure
            [ vs
            | vs <- mVs
            , _vs_parFlag vs || _vs_paragraph vs == VerticalSpacingParNone
            ]
        BDDebug s bd -> do
          r <- rec bd
          tellDebugMess
            $ "getSpacings: BDDebug "
            ++ show s
            ++ " (node-id="
            ++ show brDcId
            ++ "): vs="
            ++ show (take 9 r)
          pure r
      pure result
    maxVs :: [VerticalSpacing] -> VerticalSpacing
    maxVs = L.foldl'
      (\(VerticalSpacing x1 x2 _) (VerticalSpacing y1 y2 _) -> VerticalSpacing
        (max x1 y1)
        (case (x2, y2) of
          (x,                          VerticalSpacingParNone)     -> x
          (VerticalSpacingParNone,     x)                          -> x
          (VerticalSpacingParAlways i, VerticalSpacingParAlways j) ->
            VerticalSpacingParAlways $ max i j
          (VerticalSpacingParAlways i, VerticalSpacingParSome j)   ->
            VerticalSpacingParAlways $ max i j
          (VerticalSpacingParSome i,   VerticalSpacingParAlways j) ->
            VerticalSpacingParAlways $ max i j
          (VerticalSpacingParSome x,   VerticalSpacingParSome y)   ->
            VerticalSpacingParSome $ max x y)
        False)
      (VerticalSpacing 0 VerticalSpacingParNone False)
    sumVs :: [VerticalSpacing] -> VerticalSpacing
    sumVs sps = L.foldl' go initial sps
      where
        go (VerticalSpacing x1 x2 x3) (VerticalSpacing y1 y2 _) = VerticalSpacing
          (x1 + y1)
          (case (x2, y2) of
            (x,                          VerticalSpacingParNone)     -> x
            (VerticalSpacingParNone,     x)                          -> x
            (VerticalSpacingParAlways i, VerticalSpacingParAlways j) ->
              VerticalSpacingParAlways $ i + j
            (VerticalSpacingParAlways i, VerticalSpacingParSome j)   ->
              VerticalSpacingParAlways $ i + j
            (VerticalSpacingParSome i,   VerticalSpacingParAlways j) ->
              VerticalSpacingParAlways $ i + j
            (VerticalSpacingParSome x,   VerticalSpacingParSome y)   ->
              VerticalSpacingParSome $ x + y
          )
          x3
        singleline x = _vs_paragraph x == VerticalSpacingParNone
        isPar x = _vs_parFlag x
        parFlag = case sps of
          [] -> True
          _ -> all singleline (L.init sps) && isPar (L.last sps)
        initial = VerticalSpacing 0 VerticalSpacingParNone parFlag
    getMaxVS :: VerticalSpacing -> Int
    getMaxVS (VerticalSpacing x1 x2 _) = x1 `max` case x2 of
      VerticalSpacingParSome i   -> i
      VerticalSpacingParNone     -> 0
      VerticalSpacingParAlways i -> i
    spMakePar :: VerticalSpacing -> VerticalSpacingPar
    spMakePar (VerticalSpacing x1 x2 _) = case x2 of
      VerticalSpacingParSome i   -> VerticalSpacingParSome $ x1 `max` i
      VerticalSpacingParNone     -> VerticalSpacingParSome $ x1
      VerticalSpacingParAlways i -> VerticalSpacingParAlways $ x1 `max` i

fixIndentationForMultiple
  :: (MonadMultiReader (CConfig Identity) m) => AltCurPos -> BrIndent -> m Int
fixIndentationForMultiple acp indent = do
  indAmount <- confUnpack . _lconfig_indentAmount . _conf_layout <$> mAsk
  let indAddRaw = case indent of
        BrIndentNone      -> 0
        BrIndentRegular   -> indAmount
        BrIndentSpecial i -> i
  -- for IndentPolicyMultiple, we restrict the amount of added
  -- indentation in such a manner that we end up on a multiple of the
  -- base indentation.
  indPolicy <- confUnpack . _lconfig_indentPolicy . _conf_layout <$> mAsk
  pure $
    if indPolicy == IndentPolicyMultiple
    then
      let indAddMultiple1 =
            indAddRaw - ((_acp_indent acp + indAddRaw) `mod` indAmount)
          indAddMultiple2 =
            if indAddMultiple1 <= 0
            then indAddMultiple1 + indAmount
            else indAddMultiple1
      in indAddMultiple2
    else indAddRaw
