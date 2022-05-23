----------------------------------------------------------------------------
-- |
-- Module      :  Data.RList
-- Copyright   :  (c) Sergey Vinokurov 2022
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-implicit-prelude #-}

module Data.List.Reversed
  ( RList(..)
  , empty
  , snoc
  , singleton
  , fromList
  , Data.List.Reversed.toList
  , spanMaybe
  ) where

import Data.Foldable
import GHC.Generics (Generic)

data RList a = Nil | Snoc (RList a) a
  deriving (Eq, Ord, Show, Generic, Functor, Traversable)

instance Foldable RList where
  foldMap f = go
    where
      go = \case
        Nil       -> mempty
        Snoc xs x -> go xs <> f x
  toList = Data.List.Reversed.toList

instance Semigroup (RList a) where
  Nil <> ys        = ys
  xs  <> Nil       = xs
  xs  <> Snoc ys y = Snoc (xs <> ys) y

instance Monoid (RList a) where
  mempty = empty

empty :: RList a
empty = Nil

snoc :: RList a -> a -> RList a
snoc = Snoc

singleton :: a -> RList a
singleton = Snoc Nil

fromList :: forall a. [a] -> RList a
fromList = go Nil
  where
    go :: RList a -> [a] -> RList a
    go acc []       = acc
    go acc (x : xs) = go (Snoc acc x) xs

toList :: forall a. RList a -> [a]
toList = go []
  where
    go :: [a] -> RList a -> [a]
    go acc Nil         = acc
    go acc (Snoc ys y) = go (y : acc) ys

-- span p xs ~= (dropWhile (isNothing . p) xs, takeWhile (isJust . p) xs)
spanMaybe :: (a -> Maybe b) -> RList a -> (RList a, RList b)
spanMaybe f = go
  where
    go = \case
      Nil             -> (Nil, Nil)
      xs'@(Snoc xs x) -> case f x of
        Nothing -> (xs', Nil)
        Just y  -> (rest, Snoc ys y)
          where
            (rest, ys) = go xs
