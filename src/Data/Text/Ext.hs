-- |
-- Module:     Data.Text.Ext
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Text.Ext
  ( unsafeFromShortByteString
  , fromFastString
  ) where

import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Text (Text)
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as T

import GHC.Data.FastString

unsafeFromShortByteString :: ShortByteString -> Text
unsafeFromShortByteString str@(SBS.SBS barr) =
  T.Text (TA.ByteArray barr) 0 (SBS.length str)

fromFastString :: FastString -> Text
fromFastString = unsafeFromShortByteString . fastStringToShortByteString
