{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Parser.Annotation.Ext where

import Data.Bifunctor
-- import GHC.Parser.Annotation
import GHC.Types.SrcLoc

instance Bifunctor GenLocated where
  bimap f g (L a b) = L (f a) (g b)
