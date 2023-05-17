{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.Utils
  ( parDoc
  , parDocW
  , showSDoc_
  , showOutputable
  , fromMaybeIdentity
  , fromOptionIdentity
  , Max(..)
  , ShowIsId(..)
  , A(..)
  , tellDebugMess
  , tellDebugMessShow
  , mModify
  , breakEither
  , spanMaybe
  , FirstLastView(..)
  , splitFirstLast
  , transformUp
  , transformDownMay
  ) where

import Data.Coerce qualified
import Data.Data
import Data.Functor
import Data.Generics.Uniplate.Direct qualified as Uniplate
import Data.Sequence qualified as Seq
import GHC.Driver.Ppr qualified as GHC
import GHC.OldList qualified as List
import GHC.Utils.Outputable qualified as GHC
import Language.Haskell.Brittany.Internal.Prelude
import Text.PrettyPrint qualified as PP

parDoc :: String -> PP.Doc
parDoc = PP.fsep . fmap PP.text . List.words

parDocW :: [String] -> PP.Doc
parDocW = PP.fsep . fmap PP.text . List.words . List.unwords

showSDoc_ :: GHC.SDoc -> String
showSDoc_ = GHC.showSDocUnsafe

showOutputable :: GHC.Outputable a => a -> String
showOutputable = GHC.showPprUnsafe

fromMaybeIdentity :: Identity a -> Maybe a -> Identity a
fromMaybeIdentity x y = Data.Coerce.coerce $ fromMaybe (Data.Coerce.coerce x) y

fromOptionIdentity :: Identity a -> Maybe a -> Identity a
fromOptionIdentity x y =
  Data.Coerce.coerce $ fromMaybe (Data.Coerce.coerce x) y

-- maximum monoid over N+0
-- or more than N, because Num is allowed.
newtype Max a = Max { getMax :: a }
  deriving (Eq, Ord, Show, Bounded, Num)

instance (Num a, Ord a) => Semigroup (Max a) where
  (<>) = Data.Coerce.coerce (max :: a -> a -> a)

instance (Num a, Ord a) => Monoid (Max a) where
  mempty = Max 0
  mappend = (<>)

newtype ShowIsId = ShowIsId String deriving Data

instance Show ShowIsId where
  show (ShowIsId x) = x

data A x = A ShowIsId x
  deriving Data

tellDebugMess :: MonadMultiWriter (Seq String) m => String -> m ()
tellDebugMess s = mTell $ Seq.singleton s

tellDebugMessShow
  :: forall a m . (MonadMultiWriter (Seq String) m, Show a) => a -> m ()
tellDebugMessShow = tellDebugMess . show

-- i should really put that into multistate..
mModify :: MonadMultiState s m => (s -> s) -> m ()
mModify f = mGet >>= mSet . f

breakEither :: (a -> Either b c) -> [a] -> ([b], [c])
breakEither _ [] = ([], [])
breakEither fn (a1 : aR) = case fn a1 of
  Left  b -> (b : bs, cs)
  Right c -> (bs, c : cs)
  where
    (bs, cs) = breakEither fn aR

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f (x1 : xR) | Just y <- f x1 = (y : ys, xs)
  where
    (ys, xs) = spanMaybe f xR
spanMaybe _ xs = ([], xs)

data FirstLastView a
  = FirstLastEmpty
  | FirstLastSingleton a
  | FirstLast a [a] a

splitFirstLast :: [a] -> FirstLastView a
splitFirstLast []        = FirstLastEmpty
splitFirstLast [x]       = FirstLastSingleton x
splitFirstLast (x1 : xr) = FirstLast x1 (List.init xr) (List.last xr)

-- TODO: move to uniplate upstream?
-- aka `transform`
transformUp :: Uniplate.Uniplate on => (on -> on) -> (on -> on)
transformUp f = g
  where
    g = f . Uniplate.descend g

_transformDown :: Uniplate.Uniplate on => (on -> on) -> (on -> on)
_transformDown f = g
  where
    g = Uniplate.descend g . f

transformDownMay :: Uniplate.Uniplate on => (on -> Maybe on) -> (on -> on)
transformDownMay f = g
  where
    g x = maybe x (Uniplate.descend g) $ f x

_transformDownRec :: Uniplate.Uniplate on => (on -> Maybe on) -> (on -> on)
_transformDownRec f = g
  where
    g x = maybe (Uniplate.descend g x) g $ f x
