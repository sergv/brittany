{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Language.Haskell.Brittany.Internal.BackendUtils
  ( layoutWriteAppend
  , layoutWriteAppendSpaces
  , layoutWriteAppendMultiline
  , layoutWriteNewlineBlock
  , layoutSetCommentCol
  , layoutMoveToCommentPos
  , layoutWriteNewline
  , layoutWriteEnsureNewlineBlock
  , layoutWriteEnsureAbsoluteN
  , layoutBaseYPushInternal
  , layoutBaseYPopInternal
  , layoutIndentLevelPushInternal
  , layoutIndentLevelPopInternal
  , layoutRemoveIndentLevelLinger
  , layoutWithAddBaseCol
  , layoutWithAddBaseColBlock
  , layoutWithAddBaseColNBlock
  , layoutWriteEnsureBlock
  , layoutWithAddBaseColN
  , layoutBaseYPushCur
  , layoutBaseYPop
  , layoutIndentLevelPushCur
  , layoutIndentLevelPop
  , layoutAddSepSpace
  , moveToExactAnn
  , moveNextLine
  , ppmMoveToExactLocAnchor
  , layoutIndentRestorePostComment

  , moveByDelta
  ) where

import Prelude hiding (lines)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.MultiRWS (MonadMultiReader(..), MonadMultiState(..), MonadMultiWriter(..), mGet)
import Data.Foldable
import Data.List qualified as L
import Data.Maybe
import Data.Semigroup (Semigroup(..), Last(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as TLB
import GHC.Parser.Annotation
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils

stimes' :: Monoid a => Int -> a -> a
stimes' n x
  | n < 1     = mempty
  | otherwise = stimes n x

layoutWriteAppend
  :: (MonadMultiWriter TLB.Builder m, MonadMultiState LayoutState m)
  => Text
  -> m ()
layoutWriteAppend t = do
  state <- mGet
  case _lstate_curYOrAddNewline state of
    InsertNewlines i -> mTell $ stimes' i $ TLB.singleton '\n'
    Cols{}           -> pure ()
  let spaces = fromMaybe 0 $ _lstate_addSepSpace state
  mTell $ stimes' spaces $ TLB.singleton ' '
  mTell $ TLB.fromText t
  mModify $ \s -> s
    { _lstate_curYOrAddNewline = Cols $ T.length t + spaces + case _lstate_curYOrAddNewline s of
      Cols c           -> c
      InsertNewlines{} -> 0
    , _lstate_addSepSpace = Nothing
    }

layoutWriteAppendSpaces
  :: (MonadMultiWriter TLB.Builder m, MonadMultiState LayoutState m)
  => Int
  -> m ()
layoutWriteAppendSpaces i =
  unless (i == 0) $ do
    state <- mGet
    mSet $ state
      { _lstate_addSepSpace = Just $ maybe i (+ i) $ _lstate_addSepSpace state
      }

layoutWriteAppendMultiline
  :: (MonadMultiWriter TLB.Builder m, MonadMultiState LayoutState m)
  => [Text]
  -> m ()
layoutWriteAppendMultiline ts =
  case ts of
    []     -> layoutWriteAppend (T.pack "") -- need to write empty, too.
    l : lr -> do
      layoutWriteAppend l
      for_ lr $ \x -> do
        layoutWriteNewline
        layoutWriteAppend x

-- adds a newline and adds spaces to reach the base column.
layoutWriteNewlineBlock
  :: (MonadMultiWriter TLB.Builder m, MonadMultiState LayoutState m)
  => m ()
layoutWriteNewlineBlock = do
  state <- mGet
  mSet $ state
    { _lstate_curYOrAddNewline = InsertNewlines 1
    , _lstate_addSepSpace      = Just $ lstate_baseY state
    }

layoutSetCommentCol :: MonadMultiState LayoutState m => m ()
layoutSetCommentCol = do
  state <- mGet
  let col = case _lstate_curYOrAddNewline state of
        Cols i           -> i + fromMaybe 0 (_lstate_addSepSpace state)
        InsertNewlines{} -> lstate_baseY state
  unless (Data.Maybe.isJust $ _lstate_commentCol state)
    $ mSet state { _lstate_commentCol = Just col }

-- This is also used to move to non-comments in a couple of places. Seems
-- to be harmless so far..
layoutMoveToCommentPos
  :: (MonadMultiWriter TLB.Builder m, MonadMultiState LayoutState m)
  => Int
  -> Int
  -> Int
  -> m ()
layoutMoveToCommentPos y x commentLines = do
  state <- mGet
  mSet state
    { _lstate_curYOrAddNewline = case _lstate_curYOrAddNewline state of
      Cols i           -> if y == 0 then Cols i else InsertNewlines y
      InsertNewlines{} -> InsertNewlines y
    , _lstate_addSepSpace =
      Just $ case _lstate_commentCol state of
        Just _  -> case _lstate_curYOrAddNewline state of
          Cols{}           -> if y == 0 then x else _lstate_indLevelLinger state + x
          InsertNewlines{} -> _lstate_indLevelLinger state + x
        Nothing -> if y == 0 then x else _lstate_indLevelLinger state + x
    , _lstate_commentCol = Just $ case _lstate_commentCol state of
      Just existing -> existing
      Nothing       -> case _lstate_curYOrAddNewline state of
        Cols i           -> i + fromMaybe 0 (_lstate_addSepSpace state)
        InsertNewlines{} -> lstate_baseY state
    , _lstate_commentNewlines =
      _lstate_commentNewlines state + y + commentLines - 1
    }

-- | does _not_ add spaces to again reach the current base column.
layoutWriteNewline
  :: (MonadMultiWriter TLB.Builder m, MonadMultiState LayoutState m)
  => m ()
layoutWriteNewline = do
  state <- mGet
  mSet $ state
    { _lstate_curYOrAddNewline = case _lstate_curYOrAddNewline state of
      Cols{}           -> InsertNewlines 1
      InsertNewlines i -> InsertNewlines (i + 1)
    , _lstate_addSepSpace = Nothing
    }

_layoutResetCommentNewlines :: MonadMultiState LayoutState m => m ()
_layoutResetCommentNewlines =
  mModify $ \state -> state { _lstate_commentNewlines = 0 }

layoutWriteEnsureNewlineBlock
  :: (MonadMultiWriter TLB.Builder m, MonadMultiState LayoutState m)
  => m ()
layoutWriteEnsureNewlineBlock = do
  state <- mGet
  mSet $ state
    { _lstate_curYOrAddNewline = case _lstate_curYOrAddNewline state of
      Cols{}           -> InsertNewlines 1
      InsertNewlines i -> InsertNewlines $ max 1 i
    , _lstate_addSepSpace = Just $ lstate_baseY state
    , _lstate_commentCol = Nothing
    }

layoutWriteEnsureAbsoluteN
  :: (MonadMultiWriter TLB.Builder m, MonadMultiState LayoutState m)
  => Int
  -> m ()
layoutWriteEnsureAbsoluteN n = do
  state <- mGet
  let diff = case (_lstate_commentCol state, _lstate_curYOrAddNewline state) of
        (Just c,  _)                -> n - c
        (Nothing, Cols i)           -> n - i
        (Nothing, InsertNewlines{}) -> n
  when (diff > 0) $
    mSet $ state { _lstate_addSepSpace = Just diff } -- this always sets to
                                            -- at least (Just 1), so we won't
                                            -- overwrite any old value in any
                                            -- bad way.

layoutBaseYPushInternal :: MonadMultiState LayoutState m => Int -> m ()
layoutBaseYPushInternal i =
  mModify $ \s -> s { _lstate_baseYs = i : _lstate_baseYs s }

layoutBaseYPopInternal :: MonadMultiState LayoutState m => m ()
layoutBaseYPopInternal =
  mModify $ \s -> s { _lstate_baseYs = L.drop 1 $ _lstate_baseYs s }

layoutIndentLevelPushInternal :: MonadMultiState LayoutState m => Int -> m ()
layoutIndentLevelPushInternal i =
  mModify $ \s -> s
    { _lstate_indLevelLinger = lstate_indLevel s
    , _lstate_indLevels      = i : _lstate_indLevels s
    }

layoutIndentLevelPopInternal :: MonadMultiState LayoutState m => m ()
layoutIndentLevelPopInternal =
  mModify $ \s -> s
    { _lstate_indLevelLinger = lstate_indLevel s
    , _lstate_indLevels      = L.drop 1 $ _lstate_indLevels s
    }

layoutRemoveIndentLevelLinger :: MonadMultiState LayoutState m => m ()
layoutRemoveIndentLevelLinger =
  mModify $ \s -> s { _lstate_indLevelLinger = lstate_indLevel s }

layoutWithAddBaseCol
  :: ( MonadMultiWriter TLB.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiReader Config m
     )
  => m ()
  -> m ()
layoutWithAddBaseCol m = do
  amount <- confUnpack . _lconfig_indentAmount . _conf_layout <$> mAsk
  state  <- mGet
  layoutBaseYPushInternal $ lstate_baseY state + amount
  m
  layoutBaseYPopInternal

layoutWithAddBaseColBlock
  :: ( MonadMultiWriter TLB.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiReader Config m
     )
  => m ()
  -> m ()
layoutWithAddBaseColBlock m = do
  amount <- confUnpack . _lconfig_indentAmount . _conf_layout <$> mAsk
  state  <- mGet
  layoutBaseYPushInternal $ lstate_baseY state + amount
  layoutWriteEnsureBlock
  m
  layoutBaseYPopInternal

layoutWithAddBaseColNBlock
  :: (MonadMultiWriter TLB.Builder m, MonadMultiState LayoutState m)
  => Int
  -> m ()
  -> m ()
layoutWithAddBaseColNBlock amount m = do
  state <- mGet
  layoutBaseYPushInternal $ lstate_baseY state + amount
  layoutWriteEnsureBlock
  m
  layoutBaseYPopInternal

layoutWriteEnsureBlock
  :: (MonadMultiWriter TLB.Builder m, MonadMultiState LayoutState m)
  => m ()
layoutWriteEnsureBlock = do
  state <- mGet
  let diff = case (_lstate_addSepSpace state, _lstate_curYOrAddNewline state) of
        (Nothing, Cols i)           -> lstate_baseY state - i
        (Nothing, InsertNewlines{}) -> lstate_baseY state
        (Just sp, Cols i)           -> max sp (lstate_baseY state - i)
        (Just sp, InsertNewlines{}) -> max sp (lstate_baseY state)
  -- when (diff>0) $ layoutWriteNewlineBlock
  when (diff > 0) $ do
    mSet $ state { _lstate_addSepSpace = Just $ diff }

layoutWithAddBaseColN
  :: (MonadMultiWriter TLB.Builder m, MonadMultiState LayoutState m)
  => Int
  -> m ()
  -> m ()
layoutWithAddBaseColN amount m = do
  state <- mGet
  layoutBaseYPushInternal $ lstate_baseY state + amount
  m
  layoutBaseYPopInternal

layoutBaseYPushCur :: MonadMultiState LayoutState m => m ()
layoutBaseYPushCur = do
  state <- mGet
  case _lstate_commentCol state of
    Nothing ->
      case (_lstate_curYOrAddNewline state, _lstate_addSepSpace state) of
        (Cols i,           Just j)  -> layoutBaseYPushInternal (i + j)
        (Cols i,           Nothing) -> layoutBaseYPushInternal i
        (InsertNewlines{}, _)       -> layoutBaseYPushInternal $ lstate_baseY state
    Just cCol -> layoutBaseYPushInternal cCol

layoutBaseYPop :: MonadMultiState LayoutState m => m ()
layoutBaseYPop =
  layoutBaseYPopInternal

layoutIndentLevelPushCur :: MonadMultiState LayoutState m => m ()
layoutIndentLevelPushCur = do
  state <- mGet
  let y = case (_lstate_curYOrAddNewline state, _lstate_addSepSpace state) of
        (Cols i,           Just j)  -> i + j
        (Cols i,           Nothing) -> i
        (InsertNewlines{}, Just j)  -> j
        (InsertNewlines{}, Nothing) -> 0
  layoutIndentLevelPushInternal y

layoutIndentLevelPop :: MonadMultiState LayoutState m => m ()
layoutIndentLevelPop = do
  layoutIndentLevelPopInternal
  -- why are comment indentations relative to the previous indentation on
  -- the first node of an additional indentation, and relative to the outer
  -- indentation after the last node of some indented stuff? sure does not
  -- make sense.
  layoutRemoveIndentLevelLinger

layoutAddSepSpace :: MonadMultiState LayoutState m => m ()
layoutAddSepSpace = do
  state <- mGet
  mSet $ state
    { _lstate_addSepSpace = Just $ fromMaybe 1 $ _lstate_addSepSpace state
    }

-- TODO: when refactoring is complete, the other version of this method
-- can probably be removed.
moveToExactAnn
  :: MonadMultiState LayoutState m
  => EpAnn ann
  -> m ()
moveToExactAnn a = moveNextLine $
  case a of
    EpAnn{entry} -> case anchor_op entry of
      UnchangedAnchor -> 0
      MovedAnchor dp  -> getDeltaLine dp
    EpAnnNotUsed -> 0

moveNextLine :: MonadMultiState LayoutState m => Int -> m ()
moveNextLine delta = mModify $ \state ->
  let upd = case _lstate_curYOrAddNewline state of
        old@Cols{}       -> if delta == 0 then old else InsertNewlines delta
        InsertNewlines i -> InsertNewlines $ max delta i
  in
    state
      { _lstate_curYOrAddNewline = upd
      , _lstate_addSepSpace      =
        case upd of
          Cols{}           -> Nothing
          InsertNewlines{} ->
            _lstate_commentCol state <|> _lstate_addSepSpace state <|> Just (lstate_baseY state)
      , _lstate_commentCol       = Nothing
      }

ppmMoveToExactLocAnchor
  :: MonadMultiWriter TLB.Builder m => Anchor -> m ()
ppmMoveToExactLocAnchor an =
  case anchor_op an of
    UnchangedAnchor -> pure ()
    MovedAnchor dp  ->
      mTell $ stimes' lines (TLB.singleton '\n') <> stimes' cols (TLB.singleton ' ')
      where
        (lines, cols) = unpackDeltaPos dp

moveByDelta
  :: MonadMultiWriter TLB.Builder m => Maybe Delta -> m ()
moveByDelta = traverse_ $ \Delta{dLines, dCols} ->
  mTell $ stimes' dLines (TLB.singleton '\n') <> stimes' dCols (TLB.singleton ' ')

layoutIndentRestorePostComment
  :: (MonadMultiState LayoutState m, MonadMultiWriter TLB.Builder m)
  => m ()
layoutIndentRestorePostComment = do
  state <- mGet
  let mCommentCol = _lstate_commentCol state
      eCurYAddNL  = _lstate_curYOrAddNewline state
  mModify
    $ \s -> s { _lstate_commentCol = Nothing, _lstate_commentNewlines = 0 }
  case (mCommentCol, eCurYAddNL) of
    (Just commentCol, Cols{}) -> do
      layoutWriteEnsureNewlineBlock
      layoutWriteEnsureAbsoluteN $ commentCol + fromMaybe
        0
        (_lstate_addSepSpace state)
    _ -> pure ()
