----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Brittany.Internal.Formatting
-- Copyright   :  (c) Sergey Vinokurov 2022
----------------------------------------------------------------------------

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Brittany.Internal.Formatting
  ( format
  , ParseError(..)
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Semigroup
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type qualified as GHC
import Language.Haskell.Brittany.Internal
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Obfuscation
import Language.Haskell.Brittany.Internal.ParseModule
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.GHC.ExactPrint qualified as ExactPrint

newtype ParseError = ParseError { unParseError :: String }

isPreprocessorLine :: Text -> Bool
isPreprocessorLine s = case T.uncons s of
  Just ('#', rest) -> "include" `T.isPrefixOf` T.stripStart rest
  _                -> False

commentOutPreprocessor :: Text -> Text
commentOutPreprocessor =
  T.intercalate "\n" . map f . T.splitOn "\n"
  where
    f s | isPreprocessorLine s = "-- BRITANY_INCLUDE_HACK " <> s
        | otherwise          = s

uncommmentPreprocessor :: Text -> Text
uncommmentPreprocessor =
  T.intercalate "\n" . map f . T.splitOn "\n"
  where
    f s = fromMaybe s $ T.stripPrefix "-- BRITANY_INCLUDE_HACK " s

cppCheckFunc :: MonadIO m => CPPMode -> GHC.DynFlags -> m (Either String (Bool, Maybe BrittanyWarning))
cppCheckFunc cppMode dynFlags =
  pure $
    if GHC.xopt GHC.Cpp dynFlags
    then
      case cppMode of
        CPPModeAbort ->
          Left "Encountered -XCPP. Aborting."
        CPPModeWarn ->
          Right (True, Just CPPWarning)
        CPPModeNowarn ->
          Right (True, Nothing)
    else
      Right (False, Nothing)

-- | Reformat given source according to style specified by Config.
format
  :: Config   -- ^ Formatting config
  -> FilePath -- ^ Input filename
  -> Text     -- ^ Source program to format
  -> IO (Either ParseError (Text, [BrittanyWarning], [BrittanyError], Seq String, Bool))
format config fname input = do
  parseResult <-
    parseModuleFromString ghcOptions fname (cppCheckFunc cppMode) (T.unpack input')
  case parseResult of
    Left err -> pure $ Left $ ParseError err
    Right (parsedSource, (hasCPP, cppWarns)) -> do
      -- TODO: collect module config here and merge with values from global config
      let moduleConf = config
          disableFormatting :: Bool
          disableFormatting = confUnpack (_conf_disable_formatting moduleConf)

      (formatted, errs, logs, hasChanges) <-
        if
          | disableFormatting -> do
            pure (input, [], Seq.empty, False)
          | exactprintOnly -> do
            let r = T.pack $ ExactPrint.exactPrint parsedSource
            pure (r, [], Seq.empty, r /= input)
          | otherwise -> do
            let omitCheck :: Bool
                omitCheck = confUnpack (_econf_omit_output_valid_check (_conf_errorHandling moduleConf))
            (outRaw, ews, logs) <-
              if hasCPP || omitCheck
              then pure $ pPrintModule moduleConf parsedSource
              else pPrintModuleAndCheck moduleConf parsedSource
            let out | workAroundCPP = uncommmentPreprocessor outRaw
                    | otherwise     = outRaw
            out' <-
              if confUnpack (_conf_obfuscate moduleConf)
              then obfuscate out
              else pure out
            pure (out', ews, logs, out' /= input)

      pure $ Right (formatted, maybeToList cppWarns, errs, logs, hasChanges)
  where
    input'
      | workAroundCPP && not exactprintOnly = commentOutPreprocessor input
      | otherwise                           = input

    ghcOptions :: [String]
    ghcOptions = confUnpack (_options_ghc (_conf_forward config))

    cppMode :: CPPMode
    cppMode = confUnpack (_ppconf_CPPMode (_conf_preprocessor config))

    workAroundCPP :: Bool
    workAroundCPP = confUnpack (_ppconf_hackAroundIncludes (_conf_preprocessor config))

    exactprintOnly :: Bool
    exactprintOnly = viaGlobal
      where
        viaGlobal :: Bool
        viaGlobal = confUnpack (_conf_roundtrip_exactprint_only config)

      -- putStrErrLn
      --   $ "Warning: Encountered -XCPP."
      --   ++ " Be warned that -XCPP is not supported and that"
      --   ++ " brittany cannot check that its output is syntactically"
      --   ++ " valid in its presence."

