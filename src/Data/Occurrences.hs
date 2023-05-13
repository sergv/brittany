----------------------------------------------------------------------------
-- |
-- Module      :  Data.Occurrences
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-implicit-prelude #-}
{-# OPTIONS_GHC -Wno-orphans          #-}

module Data.Occurrences
  ( Occurrences(..)
  ) where

import qualified Data.ByteString as BS
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics

import qualified GHC.Data.Strict
import GHC.Parser.Annotation
import GHC.Types.SrcLoc

import GHC.Hs.DocString

-- | This class is responsible for locating all occurrences of type 'a' within type 'b',
-- passing them to the provided function and collecting all its results via 'mappend'.
class Occurrences (a :: Type) (b :: Type) where
  foldAllOccurrences :: Monoid m => (a -> m) -> b -> m
  {-# INLINE foldAllOccurrences #-}
  default foldAllOccurrences :: (Generic b, GOccurrences a (Rep b), Monoid m) => (a -> m) -> b -> m
  foldAllOccurrences f = gfoldAllOccurrences f . from

class GOccurrences (a :: Type) (b :: Type -> Type) where
  gfoldAllOccurrences :: Monoid m => (a -> m) -> b ix -> m

instance GOccurrences a V1 where
  {-# INLINE gfoldAllOccurrences #-}
  gfoldAllOccurrences _ = error "gfoldAllOccurrences for V1"

instance GOccurrences a U1 where
  {-# INLINE gfoldAllOccurrences #-}
  gfoldAllOccurrences _ = mempty

instance (GOccurrences a f, GOccurrences a g) => GOccurrences a (f :+: g) where
  {-# INLINE gfoldAllOccurrences #-}
  gfoldAllOccurrences f = \case
    L1 x -> gfoldAllOccurrences f x
    R1 y -> gfoldAllOccurrences f y

instance (GOccurrences a f, GOccurrences a g) => GOccurrences a (f :*: g) where
  {-# INLINE gfoldAllOccurrences #-}
  gfoldAllOccurrences f (x :*: y) = gfoldAllOccurrences f x <> gfoldAllOccurrences f y

instance GOccurrences a b => GOccurrences a (M1 x y b) where
  {-# INLINE gfoldAllOccurrences #-}
  gfoldAllOccurrences f = gfoldAllOccurrences f . unM1

instance {-# OVERLAPS #-} GOccurrences a (K1 i a) where
  {-# INLINE gfoldAllOccurrences #-}
  gfoldAllOccurrences f = f . unK1

instance {-# OVERLAPPABLE #-} Occurrences a b => GOccurrences a (K1 i b) where
  {-# INLINE gfoldAllOccurrences #-}
  gfoldAllOccurrences f = foldAllOccurrences f . unK1

deriving instance Generic (EpAnn a)
deriving instance Generic (GenLocated a b)
deriving instance Generic (SrcSpanAnn' a)
deriving instance Generic AddEpAnn
deriving instance Generic Anchor
deriving instance Generic AnchorOperation
deriving instance Generic AnnContext
deriving instance Generic AnnKeywordId
deriving instance Generic AnnList
deriving instance Generic AnnListItem
deriving instance Generic AnnParen
deriving instance Generic AnnPragma
deriving instance Generic BufPos
deriving instance Generic BufSpan
deriving instance Generic DeltaPos
deriving instance Generic EpAnnComments
deriving instance Generic EpaComment
deriving instance Generic EpaCommentTok
deriving instance Generic EpaLocation
deriving instance Generic HsDocStringChunk
deriving instance Generic HsDocStringDecorator
deriving instance Generic HsDocString
deriving instance Generic IsUnicodeSyntax
deriving instance Generic NameAdornment
deriving instance Generic NameAnn
deriving instance Generic ParenType
deriving instance Generic TrailingAnn

deriving instance Generic (GHC.Data.Strict.Maybe a)

instance {-# OVERLAPS #-} Occurrences a a where
  {-# INLINE foldAllOccurrences #-}
  foldAllOccurrences = ($)


instance {-# OVERLAPS #-} Occurrences RealSrcSpan SrcSpan where
  {-# INLINE foldAllOccurrences #-}
  foldAllOccurrences f = foldMap f . srcSpanToRealSrcSpan

instance {-# OVERLAPPABLE #-} Occurrences a SrcSpan where
  {-# INLINE foldAllOccurrences #-}
  foldAllOccurrences _ = mempty

instance {-# OVERLAPPABLE #-} Occurrences a RealSrcSpan where
  {-# INLINE foldAllOccurrences #-}
  foldAllOccurrences _ = mempty

instance {-# OVERLAPPABLE #-} Occurrences a DeltaPos where
  {-# INLINE foldAllOccurrences #-}
  foldAllOccurrences _ = mempty

instance {-# OVERLAPPABLE #-} Occurrences a Char where
  {-# INLINE foldAllOccurrences #-}
  foldAllOccurrences _ = mempty

instance {-# OVERLAPPABLE #-} Occurrences a Int where
  {-# INLINE foldAllOccurrences #-}
  foldAllOccurrences _ = mempty

instance {-# OVERLAPPABLE #-} Occurrences a BS.ByteString where
  {-# INLINE foldAllOccurrences #-}
  foldAllOccurrences _ = mempty

instance Occurrences a b => Occurrences a (EpAnn b)
instance Occurrences a b => Occurrences a (SrcSpanAnn' b)
instance (Occurrences a ann, Occurrences a b) => Occurrences a (GenLocated ann b)

instance Occurrences a AddEpAnn
instance Occurrences a Anchor
instance Occurrences a AnchorOperation
instance Occurrences a AnnContext
instance Occurrences a AnnKeywordId
instance Occurrences a AnnList
instance Occurrences a AnnListItem
instance Occurrences a AnnParen
instance Occurrences a AnnPragma
instance Occurrences a BufPos
instance Occurrences a BufSpan
instance Occurrences a EpAnnComments
instance Occurrences a EpaComment
instance Occurrences a EpaCommentTok
instance Occurrences a EpaLocation
instance Occurrences a IsUnicodeSyntax
instance Occurrences a NameAdornment
instance Occurrences a NameAnn
instance Occurrences a ParenType
instance Occurrences a TrailingAnn

instance Occurrences a b => Occurrences a (GHC.Data.Strict.Maybe b)

instance Occurrences a HsDocStringChunk
instance Occurrences a HsDocStringDecorator
instance Occurrences a HsDocString

instance Occurrences a b => Occurrences a [b]
instance Occurrences a b => Occurrences a (NonEmpty b)
instance Occurrences a b => Occurrences a (Maybe b)

instance (Occurrences a b, Occurrences a c) => Occurrences a (b, c)

