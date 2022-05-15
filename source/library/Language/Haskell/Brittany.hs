{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany
  ( staticDefaultConfig
  , forwardOptionsSyntaxExtsEnabled
  , userConfigPath
  , findLocalConfigPath
  , readConfigs
  , readConfigsWithUserConfig
  , Config
  , CConfig(..)
  , CDebugConfig(..)
  , CLayoutConfig(..)
  , CErrorHandlingConfig(..)
  , CForwardOptions(..)
  , CPreProcessorConfig(..)
  , BrittanyError(..)
  ) where

import Language.Haskell.Brittany.Internal.Config
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Types
