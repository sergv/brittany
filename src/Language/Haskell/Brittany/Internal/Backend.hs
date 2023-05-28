{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Brittany.Internal.Backend
  ( ColIndex
  , ColumnSpacing(..)
  , ColumnBlock
  , ColumnBlocks
  , ColMap1
  , ColMap2
  , ColInfo(..)
  , ColBuildState(..)
  , LayoutConstraints
  , layoutBriDocM
  , briDocLineLength
  , briDocIsMultiLine
  , alignColsLines
  , briDocToColInfo
  , getTrueSpacings
  , withAlloc
  , processInfo
  , processInfoIgnore
  ) where

import Prelude hiding (lines)

import Control.Monad
import Control.Monad.State (MonadState, get, put, evalState)
import Control.Monad.Trans.MultiRWS (MonadMultiReader(..), MonadMultiState(..), MonadMultiWriter(..), mGet)
import Control.Monad.Trans.State.Strict qualified as StateS
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.IntMap.Lazy qualified as IML
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.Maybe
import Data.Semigroup
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as TLB
import Data.Traversable

import Language.Haskell.Brittany.Internal.BackendUtils
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.RecursionSchemes
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.GHC.ExactPrint.Types qualified as ExactPrint.Types

type ColIndex = Int

data ColumnSpacing
  = ColumnSpacingLeaf !Int
  | ColumnSpacingRef !Int !Int

type ColumnBlock a = [a]
type ColumnBlocks a = Seq [a]
type ColMap1
  = IML.IntMap {- ColIndex -}
                   (Bool, ColumnBlocks ColumnSpacing)
type ColMap2
  = IML.IntMap {- ColIndex -}
                   (Float, ColumnBlock Int, ColumnBlocks Int)
                                          -- (ratio of hasSpace, maximum, raw)

data ColInfo
  = ColInfoStart -- start value to begin the mapAccumL.
  | ColInfoNo BriDoc
  | ColInfo !ColIndex ColSig [(Int, ColInfo)]

-- instance Show ColInfo where
--   show ColInfoStart = "ColInfoStart"
--   show (ColInfoNo bd) =
--     "ColInfoNo " ++ show (take 30 (show (briDocToDoc bd)) ++ "..")
--   show (ColInfo ind sig list) =
--     "ColInfo " ++ show ind ++ " " ++ show sig ++ " " ++ show list

data ColBuildState = ColBuildState
  { _cbs_map   :: !ColMap1
  , _cbs_index :: !ColIndex
  }

type LayoutConstraints m
  = ( MonadMultiReader Config m
    , MonadMultiWriter TLB.Builder m
    , MonadMultiWriter (Seq String) m
    , MonadMultiState LayoutState m
    )

doIndent :: LayoutConstraints m => BrIndent -> m () -> m ()
doIndent = \case
  BrIndentNone      -> id
  BrIndentRegular   -> layoutWithAddBaseCol
  BrIndentSpecial i -> layoutWithAddBaseColN i

layoutBriDocM :: forall m. LayoutConstraints m => BriDoc -> m ()
layoutBriDocM = para alg
  where
    alg :: BriDocF (m (), BriDoc) -> m ()
    alg = \case
      BDEmpty ->
        pure () -- can it be that simple
      BDLit t -> do
        layoutIndentRestorePostComment
        layoutRemoveIndentLevelLinger
        layoutWriteAppend t
      BDSeq list ->
        traverse_ fst list
      -- in this situation, there is nothing to do about cols.
      -- i think this one does not happen anymore with the current simplifications.
      -- BDCols cSig list | BDPar sameLine lines <- L.last list ->
      --   alignColsPar $ BDCols cSig (L.init list ++ [sameLine]) : lines
      BDCols _ list ->
        traverse_ fst list
      BDSeparator ->
        layoutAddSepSpace
      BDAddBaseY indent (bd, _) ->
        doIndent indent bd
      BDBaseYPushCur (bd, _) -> do
        layoutBaseYPushCur
        bd
      BDBaseYPop (bd, _) -> do
        bd
        layoutBaseYPop
      BDIndentLevelPushCur (bd, _) -> do
        layoutIndentLevelPushCur
        bd
      BDIndentLevelPop (bd, _) -> do
        bd
        layoutIndentLevelPop
      BDEnsureIndent indent (bd, _) ->
        doIndent indent $ do
          layoutWriteEnsureBlock
          bd
      BDPar indent (sameLine, _) (indented, _) -> do
        sameLine
        doIndent indent $ do
          layoutWriteNewlineBlock
          indented
      BDLines lines                 -> alignColsLines $ map snd lines
      BDAlt []                      -> error "empty BDAlt"
      BDAlt ((alt, _) : _)          -> alt
      BDForceMultiline (bd, _)      -> bd
      BDForceSingleline (bd, _)     -> bd
      BDForwardLineMode (bd, _)     -> bd
      BDExternal shouldAddComment t -> do
        let tlines     = T.lines $ t <> T.singleton '\n'
            tlineCount = length tlines
        when shouldAddComment $
          layoutWriteAppend
            $ T.pack "{- BRITTANY TODO SHOULD ADD COMMENT -}"
        for_ (zip [1 ..] tlines) $ \(i, l) -> do
          layoutWriteAppend l
          unless (i == tlineCount) $
            layoutWriteNewlineBlock
      BDPlain t -> layoutWriteAppend t
      BDAnnotationBefore finalDelta comments (bd, _) -> do
        state <- mGet

        for_ comments $ \BrComment{bcContents, bcDelta} -> do
          moveByDelta bcDelta
          layoutWriteAppend bcContents

        case (_lstate_curYOrAddNewline state, finalDelta) of
          (InsertNewlines{}, Just Delta{dLines}) -> moveNextLine dLines
          _                                      -> pure ()

        bd

      -- BDAnnotationBefore ann (bd, _) -> do
      --   -- data GenLocated l e = L l e
      --   -- getLoc         :: LocatedAn ann a -> SrcAnn ann
      --   -- ann            :: SrcAnn ann -> EpAnn ann
      --   -- ann            :: SrcSpanAnn' (EpAnn ann) -> EpAnn ann
      --   -- epAnnComments  :: EpAnn an -> EpAnnComments
      --   -- priorComments  :: EpAnnComments -> [LEpaComment]
      --   -- getEntryDP     :: LocatedAn ann a -> DeltaPos
      --   --
      --   -- ExactPrint.Utils.tokComment :: LEpaComment -> Comment
      --   -- commentContents . tokComment :: LEpaComment -> String
      --   -- -- This one doesn't normalise comment text: ghcCommentText :: LEpaComment -> String
      --   -- epaLocationRealSrcSpan :: EpaLocation -> RealSrcSpan
      --   -- tokComment :: LEpaComment -> Comment
      --
      --   -- data DeltaPos
      --   --   = SameLine { deltaColumn :: !Int }
      --   --   | DifferentLine
      --   --       { deltaLine   :: !Int, -- ^ deltaLine should always be > 0
      --   --         deltaColumn :: !Int
      --   --       } deriving (Show,Eq,Ord,Data)
      --
      --   state <- mGet
      --   let moveToExactLocation :: m ()
      --       moveToExactLocation = case _lstate_curYOrAddNewline state of
      --         Cols{}           -> pure ()
      --         InsertNewlines{} -> moveToExactAnn ann
      --
      --   case priorComments $ epAnnComments ann of
      --     [] -> moveToExactLocation
      --     ps -> do
      --       -- layoutResetSepSpace
      --       for_ ps $ \commentAnn@(L pos EpaComment{}) -> do
      --         when (commentAnchor (tokComment commentAnn) /= pos) $
      --           error $ renderString $ ppDictHeader
      --             "BDAnnotationBefore"
      --             [ "commentAnchor (tokComment commentAnn)" --> commentAnchor (tokComment commentAnn)
      --             , "pos"                                   --> pos
      --             ]
      --         ppmMoveToExactLocAnchor pos
      --         -- mTell $ TLB.fromString $ commentContents $ tokComment comment
      --         layoutWriteAppend $ T.pack $ commentContents $ tokComment commentAnn
      --       moveToExactLocation
      --
      --         -- let comment = T.pack $ ExactPrint.Types.commentContents $ tokComment lcomment
      --         -- when (comment /= "(" && comment /= ")") $ do
      --         --   let commentLines = T.lines comment
      --         --       (x, y)       = unpackDeltaPos delta
      --         --   case comment of
      --         --     -- Evil hack for CPP.
      --         --     '#': _ -> layoutMoveToCommentPos y (-999) (length commentLines)
      --         --     _      -> layoutMoveToCommentPos y x (length commentLines)
      --         --   -- fixedX <- fixMoveToLineByIsNewline x
      --         --   -- replicateM_ fixedX layoutWriteNewline
      --         --   -- layoutMoveToIndentCol y
      --         --   layoutWriteAppendMultiline commentLines
      --         -- -- mModify $ \s -> s { _lstate_curYOrAddNewline = Right 0 }
      --       -- moveToExactLocationAction
      --   bd
      BDAnnotationKW _keyword (bd, _) -> do
        bd
        let comments = []
        for_ comments $ \(ExactPrint.Types.Comment commentStr _ _ _, delta) ->
          when (commentStr /= "(" && commentStr /= ")") $ do
            let commentLines = T.lines $ T.pack commentStr
                (x, y)       = unpackDeltaPos delta
            -- evil hack for CPP:
            case commentStr of
              '#' : _ -> layoutMoveToCommentPos y (-999) (length commentLines)
              _       -> layoutMoveToCommentPos y x (length commentLines)
            -- fixedX <- fixMoveToLineByIsNewline x
            -- replicateM_ fixedX layoutWriteNewline
            -- layoutMoveToIndentCol y
            layoutWriteAppendMultiline commentLines
          -- mModify $ \s -> s { _lstate_curYOrAddNewline = Right 0 }
      BDAnnotationAfter comments (bd, _) -> do
        bd
        for_ comments $ \BrComment{bcContents, bcDelta} -> do
          moveByDelta bcDelta
          layoutWriteAppend bcContents

      BDMoveToKWDP _keyword _shouldRestoreIndent (bd, _) -> do
        -- mDP <- do
        --   state <- mGet
        --   let m = _lstate_comments state
        --   let mAnn = ExactPrint.annsDP <$> Map.lookup annKey m
        --   let
        --     relevant =
        --       [ dp
        --       | Just ann <- [mAnn]
        --       , (ExactPrint.Types.G kw1, dp) <- ann
        --       , keyword == kw1
        --       ]
        --   -- mTell $ Seq.fromList [show keyword, "KWDP: " ++ show annKey ++ " " ++ show mAnn, show relevant]
        --   case relevant of
        --     [] -> pure Nothing
        --     (ExactPrint.Types.DP (y, x) : _) -> do
        --       mSet state { _lstate_commentNewlines = 0 }
        --       pure $ Just (y - _lstate_commentNewlines state, x)
        -- case mDP of
        --   Nothing -> pure ()
        --   Just (y, x) ->
        --     -- we abuse this, as we probably will print the KW next, which is
        --     -- _not_ a comment..
        --     layoutMoveToCommentPos y (if shouldRestoreIndent then x else 0) 1
        bd
      BDNonBottomSpacing _ (bd, _) -> bd
      BDSetParSpacing (bd, _)      -> bd
      BDForceParSpacing (bd, _)    -> bd
      BDDebug s (bd, _)            -> do
        mTell $ TLB.fromText $ T.pack $ "{-" ++ s ++ "-}"
        bd

briDocLineLength :: BriDoc -> Int
briDocLineLength briDoc =
  flip StateS.evalState False $ go briDoc
  -- the state encodes whether a separator was already
  -- appended at the current position.
  where
    go :: MonadState Bool m => BriDoc -> m Int
    go (Fix bd') = case bd' of
      BDEmpty                      -> pure 0
      BDLit t                      -> put False $> T.length t
      BDSeq bds                    -> sum <$> traverse go bds
      BDCols _ bds                 -> sum <$> traverse go bds
      BDSeparator                  -> get >>= \b -> put True $> if b then 0 else 1
      BDAddBaseY _ bd              -> go bd
      BDBaseYPushCur bd            -> go bd
      BDBaseYPop bd                -> go bd
      BDIndentLevelPushCur bd      -> go bd
      BDIndentLevelPop bd          -> go bd
      BDPar _ line _               -> go line
      BDAlt{}                      -> error "briDocLineLength BDAlt"
      BDForceMultiline bd          -> go bd
      BDForceSingleline bd         -> go bd
      BDForwardLineMode bd         -> go bd
      BDExternal _ t               -> pure $ T.length t
      BDPlain t                    -> pure $ T.length t
      BDAnnotationBefore _ _ bd    -> go bd
      BDAnnotationKW _ bd          -> go bd
      BDAnnotationAfter _ bd       -> go bd
      BDMoveToKWDP _ _ bd          -> go bd
      BDLines ls@(_ : _)           -> do
        x <- get
        pure $ maximum $ ls <&> \l -> evalState (go l) x
      BDLines []                   -> error "briDocLineLength BDLines []"
      BDEnsureIndent _ bd          -> go bd
      BDSetParSpacing bd           -> go bd
      BDForceParSpacing bd         -> go bd
      BDNonBottomSpacing _ bd      -> go bd
      BDDebug _ bd                 -> go bd

briDocIsMultiLine :: BriDoc -> Bool
briDocIsMultiLine = cata alg
  where
    alg :: BriDocF Bool -> Bool
    alg = \case
      BDEmpty                           -> False
      BDLit _                           -> False
      BDSeq bds                         -> or bds
      BDCols _ bds                      -> or bds
      BDSeparator                       -> False
      BDAddBaseY _ bd                   -> bd
      BDBaseYPushCur bd                 -> bd
      BDBaseYPop bd                     -> bd
      BDIndentLevelPushCur bd           -> bd
      BDIndentLevelPop bd               -> bd
      BDPar{}                           -> True
      BDAlt{}                           -> error "briDocIsMultiLine BDAlt"
      BDForceMultiline _                -> True
      BDForceSingleline bd              -> bd
      BDForwardLineMode bd              -> bd
      BDExternal _ t | [_] <- T.lines t -> False
      BDExternal{}                      -> True
      BDPlain t      | [_] <- T.lines t -> False
      BDPlain _                         -> True
      BDAnnotationBefore _ _ bd         -> bd
      BDAnnotationKW _ bd               -> bd
      BDAnnotationAfter _ bd            -> bd
      BDMoveToKWDP _ _ bd               -> bd
      BDLines (_ : _ : _)               -> True
      BDLines [_]                       -> False
      BDLines []                        -> error "briDocIsMultiLine BDLines []"
      BDEnsureIndent _ bd               -> bd
      BDSetParSpacing bd                -> bd
      BDForceParSpacing bd              -> bd
      BDNonBottomSpacing _ bd           -> bd
      BDDebug _ bd                      -> bd

-- In theory
-- =========

-- .. this algorithm works roughly in these steps:
--
-- 1. For each line, get the (nested) column info, descending as far as
--    BDCols nodes go. The column info is a (rose) tree where the leafs
--    are arbitrary (non-BDCols) BriDocs.
-- 2. Walk through the lines and compare its column info with that of its
--    predecessor. If both are non-leafs and the column "signatures" align
--    (they don't align e.g. when they are totally different syntactical
--    structures or the number of children differs), mark these parts of
--    the two tree structures as connected and recurse to its children
--    (i.e. again comparing the children in this line with the children in
--    the previous line).
-- 3. What we now have is one tree per line, and connections between "same"
--    nodes between lines. These connection can span multiple lines.
--    We next look at spacing information. This is available at the leafs,
--    but in this step we aggregate _over connections_. At the top level, this
--    gives us one piece of data: How long would each line be, if we fully
--    aligned everything (kept all connections "active"). In contrast to
--    just taking the sum of all leafs for each tree, this line length includes
--    the spaces used for alignment.
-- 4. Treat those lines where alignment would result in overflowing of the
--    column limit. This "treatment" is currently configurable, and can e.g.
--    mean:
--    a) we stop alignment alltogether,
--    b) we remove alignment just from the overflowing lines,
--    c) we reduce the number of spaces inserted in overflowing lines using
--       some technique to make them not overflow, but without reducing the
--       space insertion to zero,
--    d) don't do anything
-- 5. Actually print the lines, walking over each tree and inserting spaces
--    according to the info and decisions gathered in the previous steps.
--
-- Possible improvements
-- =====================
--
-- - If alignment is disabled for specific lines, the aggregated per-connection
--   info of those lines is still retained and not recalculated. This can
--   result in spaces being inserted to create alignment with a line that
--   would overflow and thus gets disabled entirely.
--   An better approach would be to repeat step 3 after marking overflowing
--   lines as such, and not include the overflowing spacings as references
--   for non-overflowing ones. In the simplest case one additional iteration
--   would suffice, e.g. 1-2-3-4-3-5, but it would also be possible to refine
--   this and first remove alignment in the deepest parts of the tree for
--   overflowing lines, repeating and moving upwards until no lines are
--   anymore overflowing.
--   Further, it may make sense to break up connections when overflowing would
--   occur.
-- - It may also make sense to not filter all overflowing lines, but remove
--   them one-by-one and in each step recalculate the aggregated connection
--   spacing info. Because removing one overflowing line from the calculation
--   may very well cause another previously overflowing line to not overflow
--   any longer.
--   There is also a nasty optimization problem hiding in there (find the
--   minimal amount of alignment disabling that results in no overflows)
--   but that is overkill.
--
--   (with both these improvements there would be quite some repetition between
--   steps 3 and 4, but it should be possible to ensure termination. Still,
--   performance might become an issue as such an approach is not necessarily
--   linear in bridoc size any more.)
--
-- In practice
-- ===========
--
-- .. the current implementation is somewhat sloppy. Steps 1 and 2
-- are executed in one step, step 3 already applies one strategy that disables
-- certain connections (see `_lconfig_alignmentLimit`) and step 4 does some
-- of the calculations one might expect to occur in step 3. Steps 4 and 5
-- are executed in the same recursion, too.
-- Also, _lconfig_alignmentLimit really is itself a hack that hides the issue
-- mentioned in the first "possible improvement".
alignColsLines :: LayoutConstraints m => [BriDoc] -> m ()
alignColsLines bridocs = do -- colInfos `forM_` \colInfo -> do
  -- tellDebugMess ("alignColsLines: at " ++ take 100 (show $ briDocToDoc $ head bridocs))
  curX <- do
    state <- mGet
    pure $ fromMaybe 0 (_lstate_addSepSpace state) + case _lstate_curYOrAddNewline state of
      Cols x           -> x
      InsertNewlines{} -> 0
  colMax     <- confUnpack . _lconfig_cols . _conf_layout <$> mAsk
  alignMax   <- confUnpack . _lconfig_alignmentLimit . _conf_layout <$> mAsk
  alignBreak <- confUnpack . _lconfig_alignmentBreakOnMultiline . _conf_layout <$> mAsk
  case () of
    _ ->
      sequence_
        $ L.intersperse layoutWriteEnsureNewlineBlock
        $ colInfos
        <&> processInfo colMax processedMap
     where
      (colInfos, finalState) =
        StateS.runState (mergeBriDocs bridocs) (ColBuildState IM.empty 0)
      -- maxZipper :: [Int] -> [Int] -> [Int]
      -- maxZipper [] ys = ys
      -- maxZipper xs [] = xs
      -- maxZipper (x:xr) (y:yr) = max x y : maxZipper xr yr
      colAggregation :: [Int] -> Int
      colAggregation [] = 0 -- this probably cannot happen the way we call
                            -- this function, because _cbs_map only ever
                            -- contains nonempty Seqs.
      colAggregation xs = maximum [ x | x <- xs, x <= minimum xs + alignMax' ]
        where alignMax' = max 0 alignMax

      processedMap :: ColMap2
      processedMap = fix $ \(result :: ColMap2) ->
        _cbs_map finalState <&> \(lastFlag, colSpacingss) ->
          let colss = colSpacingss <&> \spss -> case reverse spss of
                []      -> []
                xN : xR ->
                  reverse $ (if lastFlag then fLast xN else fInit xN) : fmap fInit xR
                where
                  fLast (ColumnSpacingLeaf len)  = len
                  fLast (ColumnSpacingRef len _) = len

                  fInit (ColumnSpacingLeaf len)  = len
                  fInit (ColumnSpacingRef _ i)   = case IML.lookup i result of
                    Nothing           -> 0
                    Just (_, maxs, _) -> sum maxs
              maxCols =
                fmap colAggregation $ L.transpose $ toList colss
              (_, posXs) =
                mapAccumL (\acc x -> (acc + x, acc)) curX maxCols
              counter count l =
                if L.last posXs + L.last l <= colMax
                then count + 1
                else count
              ratio :: Float
              ratio =
                fromIntegral (foldl' counter (0 :: Int) colss) / fromIntegral (length colss)
          in (ratio, maxCols, colss)

      mergeBriDocs :: [BriDoc] -> StateS.State ColBuildState [ColInfo]
      mergeBriDocs = mergeBriDocsW ColInfoStart

      mergeBriDocsW :: ColInfo -> [BriDoc] -> StateS.State ColBuildState [ColInfo]
      mergeBriDocsW _ [] = pure []
      mergeBriDocsW lastInfo (bd : bdr) = do
        info  <- mergeInfoBriDoc True lastInfo bd
        infor <- mergeBriDocsW
          -- (if alignBreak && briDocIsMultiLine bd then ColInfoStart else info)
          (if shouldBreakAfter bd then ColInfoStart else info)
          bdr
        pure $ info : infor

      -- even with alignBreak config flag, we don't stop aligning for certain
      -- ColSigs - the ones with "False" below. The main reason is that
      -- there are uses of BDCols where they provide the alignment of several
      -- consecutive full larger code segments, for example ColOpPrefix.
      -- Motivating example is
      -- > foo
      -- >   $  [ aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -- >      , bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
      -- >      ]
      -- >   ++ [ ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc ]
      -- If we break the alignment here, then all three lines for the first
      -- list move left by one, which is horrible. We really don't want to
      -- break whole-block alignments.
      -- For list, listcomp, tuple and tuples the reasoning is much simpler:
      -- alignment should not have much effect anyways, so i simply make the
      -- choice here that enabling alignment is the safer route for preventing
      -- potential glitches, and it should never have a negative effect.
      -- For RecUpdate the argument is much less clear - it is mostly a
      -- personal preference to not break alignment for those, even if
      -- multiline. Really, this should be configurable.. (TODO)
      shouldBreakAfter :: BriDoc -> Bool
      shouldBreakAfter bd = alignBreak && briDocIsMultiLine bd && case unFix bd of
        BDCols ColTyOpPrefix _         -> False
        BDCols ColPatternsFuncPrefix _ -> True
        BDCols ColPatternsFuncInfix _  -> True
        BDCols ColPatterns _           -> True
        BDCols ColCasePattern _        -> True
        BDCols ColBindingLine{} _      -> True
        BDCols ColGuard _              -> True
        BDCols ColGuardedBody _        -> True
        BDCols ColBindStmt _           -> True
        BDCols ColDoLet _              -> True
        BDCols ColRec _                -> False
        BDCols ColRecUpdate _          -> False
        BDCols ColRecDecl _            -> False
        BDCols ColListComp _           -> False
        BDCols ColList _               -> False
        BDCols ColApp{} _              -> True
        BDCols ColTuple _              -> False
        BDCols ColTuples _             -> False
        BDCols ColOpPrefix _           -> False
        _                              -> True

      mergeInfoBriDoc
        :: Bool
        -> ColInfo
        -> BriDoc
        -> StateS.StateT ColBuildState Identity ColInfo
      mergeInfoBriDoc lastFlag ColInfoStart = briDocToColInfo lastFlag
      mergeInfoBriDoc lastFlag ColInfoNo{} = briDocToColInfo lastFlag
      mergeInfoBriDoc lastFlag (ColInfo infoInd infoSig subLengthsInfos) =
        \case
          brdc@(Fix (BDCols colSig subDocs))
            | infoSig == colSig && length subLengthsInfos == length subDocs -> do
              let
                isLastList = if lastFlag
                  then (== length subDocs) <$> [1 ..]
                  else repeat False
              infos <- zip3 isLastList (snd <$> subLengthsInfos) subDocs
                `forM` \(lf, info, bd) -> mergeInfoBriDoc lf info bd
              let curLengths = briDocLineLength <$> subDocs
              let trueSpacings = getTrueSpacings (zip curLengths infos)
              do -- update map
                s <- StateS.get
                let m = _cbs_map s
                case IM.lookup infoInd m of
                  Nothing          -> error $ "No column info for index " ++ show infoInd
                  Just (_, spaces) ->
                    StateS.put s
                      { _cbs_map = IM.insert
                        infoInd
                        (lastFlag, spaces Seq.|> trueSpacings)
                        m
                      }
              pure $ ColInfo infoInd colSig (zip curLengths infos)
            | otherwise -> briDocToColInfo lastFlag brdc
          brdc -> pure $ ColInfoNo brdc

briDocToColInfo :: Bool -> BriDoc -> StateS.State ColBuildState ColInfo
briDocToColInfo lastFlag = \case
  Fix (BDCols sig list) -> withAlloc lastFlag $ \ind -> do
    let isLastList   =
          (if lastFlag then drop 1 (False <$ list) ++ [True] else []) ++ repeat False
    subInfos <- zip isLastList list `forM` uncurry briDocToColInfo
    let lengthInfos  = zip (briDocLineLength <$> list) subInfos
        trueSpacings = getTrueSpacings lengthInfos
    pure $ (Seq.singleton trueSpacings, ColInfo ind sig lengthInfos)
  bd                    -> pure $ ColInfoNo bd

getTrueSpacings :: [(Int, ColInfo)] -> [ColumnSpacing]
getTrueSpacings lengthInfos = lengthInfos <&> \case
  (len, ColInfo i _ _) -> ColumnSpacingRef len i
  (len, _)             -> ColumnSpacingLeaf len

withAlloc
  :: Bool
  -> (ColIndex -> StateS.State ColBuildState (ColumnBlocks ColumnSpacing, ColInfo))
  -> StateS.State ColBuildState ColInfo
withAlloc lastFlag f = do
  cbs <- StateS.get
  let ind = _cbs_index cbs
  StateS.put $ cbs { _cbs_index = ind + 1 }
  (space, info) <- f ind
  StateS.get >>= \c -> StateS.put
    $ c { _cbs_map = IM.insert ind (lastFlag, space) $ _cbs_map c }
  pure info

processInfo :: forall m. LayoutConstraints m => Int -> ColMap2 -> ColInfo -> m ()
processInfo maxSpace m = \case
  ColInfoStart       -> error "should not happen (TM)"
  ColInfoNo doc      -> layoutBriDocM doc
  ColInfo ind _ list -> do
    colMaxConf <- confUnpack . _lconfig_cols . _conf_layout <$> mAsk
    alignMode  <- confUnpack . _lconfig_columnAlignMode . _conf_layout <$> mAsk
    curX       <- do
      state <- mGet
      -- tellDebugMess ("processInfo: " ++ show (_lstate_curYOrAddNewline state) ++ " - " ++ show ((_lstate_addSepSpace state)))
      let spaceAdd = fromMaybe 0 $ _lstate_addSepSpace state
      pure $ case _lstate_curYOrAddNewline state of
        Cols i -> case _lstate_commentCol state of
          Nothing -> spaceAdd + i
          Just c  -> c
        InsertNewlines{} -> spaceAdd
    let colMax = min colMaxConf (curX + maxSpace)
    (ratio, maxCols1, _colss) <- case IM.lookup ind m of
      Nothing    -> error $ "No column info for index " ++ show ind
      Just entry -> pure entry

    let maxCols2 = list <&> \case
          (_, ColInfo i _ _) -> case IM.lookup i m of
            Nothing         -> error $ "No column info for index " ++ show i
            Just (_, ms, _) -> sum ms
          (l, _) -> l
    let maxCols = zipWith max maxCols1 maxCols2
    let (maxX, posXs) = mapAccumL (\acc x -> (acc + x, acc)) curX maxCols
    -- handle the cases that the vertical alignment leads to more than max
    -- cols:
    -- this is not a full fix, and we must correct individually in addition.
    -- because: the (at least) line with the largest element in the last
    -- column will always still overflow, because we just updated the column
    -- sizes in such a way that it works _if_ we have sizes (*factor)
    -- in each column. but in that line, in the last column, we will be
    -- forced to occupy the full vertical space, not reduced by any factor.
    let fixedPosXs = case alignMode of
          ColumnAlignModeAnimouslyScale i | maxX > colMax -> fixed <&> (+ curX)
            where
              factor :: Float
              factor  =
                -- 0.0001 as an offering to the floating point gods.
                min
                  1.0001
                  (fromIntegral (i + colMax - curX) / fromIntegral (maxX - curX))
              offsets = subtract curX <$> posXs
              fixed :: [Int]
              fixed   = map (truncate . (* factor) . fromIntegral) offsets
          _ -> posXs

        spacings = zipWith (-) (L.tail fixedPosXs ++ [min maxX colMax]) fixedPosXs

        alignAct :: m ()
        alignAct = zip3 fixedPosXs spacings list `forM_` \(destX, s, x) -> do
          layoutWriteEnsureAbsoluteN destX
          processInfo s m (snd x)

        noAlignAct :: m ()
        noAlignAct = traverse_ (processInfoIgnore . snd) list

        animousAct =
          -- trace ("animousAct fixedPosXs=" ++ show fixedPosXs ++ ", list=" ++ show list ++ ", maxSpace=" ++ show maxSpace ++ ", colMax="++show colMax) $
          if L.last fixedPosXs + fst (L.last list) > colMax
             -- per-item check if there is overflowing.
          then noAlignAct
          else alignAct
    case alignMode of
      ColumnAlignModeDisabled                        -> noAlignAct
      ColumnAlignModeUnanimously | maxX <= colMax    -> alignAct
      ColumnAlignModeUnanimously                     -> noAlignAct
      ColumnAlignModeMajority limit | ratio >= limit -> animousAct
      ColumnAlignModeMajority{}                      -> noAlignAct
      ColumnAlignModeAnimouslyScale{}                -> animousAct
      ColumnAlignModeAnimously                       -> animousAct
      ColumnAlignModeAlways                          -> alignAct

processInfoIgnore :: LayoutConstraints m => ColInfo -> m ()
processInfoIgnore = \case
  ColInfoStart     -> error "should not happen (TM)"
  ColInfoNo doc    -> layoutBriDocM doc
  ColInfo _ _ list -> traverse_ (processInfoIgnore . snd) list
