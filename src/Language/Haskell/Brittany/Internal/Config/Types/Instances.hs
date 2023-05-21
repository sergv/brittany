{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.Brittany.Internal.Config.Types.Instances () where

import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as Aeson
import Data.Yaml
import Language.Haskell.Brittany.Internal.Config.Types

aesonDecodeOptionsBrittany :: Aeson.Options
aesonDecodeOptionsBrittany = Aeson.defaultOptions
  { Aeson.omitNothingFields = True
  , Aeson.fieldLabelModifier = dropWhile (== '_')
  }

instance FromJSON IndentPolicy where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON IndentPolicy where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany
  toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

instance FromJSON AltChooser where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON AltChooser where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany
  toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

instance FromJSON ColumnAlignMode where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON ColumnAlignMode where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany
  toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

instance FromJSON CPPMode where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON CPPMode where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany
  toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

instance FromJSON ExactPrintFallbackMode where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON ExactPrintFallbackMode where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany
  toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

instance FromJSON (CLayoutConfig Maybe) where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON (CLayoutConfig Maybe) where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany

instance FromJSON (CErrorHandlingConfig Maybe) where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON (CErrorHandlingConfig Maybe) where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany

instance FromJSON (CForwardOptions Maybe) where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON (CForwardOptions Maybe) where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany

instance FromJSON (CPreProcessorConfig Maybe) where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON (CPreProcessorConfig Maybe) where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany

instance ToJSON (CConfig Maybe) where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany

-- This custom instance ensures the "omitNothingFields" behaviour not only for
-- leafs, but for nodes of the config as well. This way e.g. "{}" is valid
-- config file content.
instance FromJSON (CConfig Maybe) where
  parseJSON (Object v) =
    Config
      <$> v
      .:? Key.fromString "conf_version"
      <*> v
      .:?= Key.fromString "conf_layout"
      <*> v
      .:?= Key.fromString "conf_errorHandling"
      <*> v
      .:?= Key.fromString "conf_forward"
      <*> v
      .:?= Key.fromString "conf_preprocessor"
      <*> v
      .:? Key.fromString "conf_roundtrip_exactprint_only"
      <*> v
      .:? Key.fromString "conf_disable_formatting"
      <*> v
      .:? Key.fromString "conf_obfuscate"
  parseJSON invalid = Aeson.typeMismatch "Config" invalid

-- Pretends that the value is {} when the key is not present.
(.:?=) :: FromJSON a => Object -> Key.Key -> Parser a
o .:?= k = o .:? k >>= maybe (parseJSON (Aeson.object [])) pure
