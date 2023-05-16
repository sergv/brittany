-- |
-- Module:     Language.Haskell.Brittany.Internal.RecursionSchemes
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE LambdaCase #-}

module Language.Haskell.Brittany.Internal.RecursionSchemes
  ( Fix(..)
  , cataRewrite
  , anaDescend
  , cata
  , para
  , cataAnn
  , futu
  , futuApo
  , free2fix
  ) where

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Fix hiding (cata)

anaDescend :: Functor f => (Fix f -> Maybe (f (Fix f))) -> Fix f -> Fix f
anaDescend coalg = go
  where
    go x = maybe x (Fix . fmap go) $ coalg x

cataRewrite :: forall f. Functor f => (f (Fix f) -> Maybe (Fix f)) -> Fix f -> Fix f
cataRewrite alg = go
  where
    go = cata alg'

    alg' :: f (Fix f) -> Fix f
    alg' x = case alg x of
      Nothing -> Fix x
      Just y  -> go y

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = go
  where
    go = alg . fmap go . unFix

para :: Functor f => (f (a, Fix f) -> a) -> Fix f -> a
para alg = go
  where
    go = alg . fmap (\x' -> (go x', x')) . unFix

cataAnn :: Functor f => (a -> f b -> b) -> Cofree f a -> b
cataAnn alg = go
  where
    go (a :< x) = alg a $ fmap go x

futu :: forall f a. Functor f => (a -> f (Free f a)) -> a -> Fix f
futu alg = go
  where
    go :: a -> Fix f
    go = Fix . fmap (free2fix go) . alg

futuApo :: forall f a. Functor f => (a -> Either (Fix f) (f (Free f a))) -> a -> Fix f
futuApo alg = go
  where
    go :: a -> Fix f
    go = either id (Fix . fmap (free2fix go)) . alg

free2fix :: Functor f => (a -> Fix f) -> Free f a -> Fix f
free2fix f = go
  where
    go = \case
      Free x -> Fix $ fmap go x
      Pure y -> f y

