{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Haskell.Brittany.Internal.Config
  ( staticDefaultConfig
  , forwardOptionsSyntaxExtsEnabled
  , cmdlineConfigParser
  , readConfig
  , userConfigPath
  , findLocalConfigPath
  , readConfigs
  , readConfigsWithUserConfig
  , writeDefaultConfig
  , showConfigYaml
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Bool qualified as Bool
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as C8
import Data.CZipWith
import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Semigroup
import Data.Yaml qualified
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Config.Types.Instances ()
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Utils
import Options.Applicative
import System.Directory qualified
import System.Directory qualified as Directory
import System.FilePath.Posix qualified as FilePath

-- brittany-next-binding { lconfig_indentPolicy: IndentPolicyLeft }
staticDefaultConfig :: CConfig Identity
staticDefaultConfig = Config
  { _conf_version = coerce (1 :: Int)
  , _conf_layout  = LayoutConfig
    { _lconfig_cols                      = coerce (100 :: Int)
    , _lconfig_indentPolicy              = coerce IndentPolicyFree
    , _lconfig_indentAmount              = coerce (2 :: Int)
    , _lconfig_indentWhereSpecial        = coerce True
    , _lconfig_indentListSpecial         = coerce True
    , _lconfig_importColumn              = coerce (50 :: Int)
    , _lconfig_importAsColumn            = coerce (50 :: Int)
    , _lconfig_altChooser                = coerce (AltChooserBoundedSearch 3)
    , _lconfig_columnAlignMode           = coerce (ColumnAlignModeMajority 0.7)
    , _lconfig_alignmentLimit            = coerce (30 :: Int)
    , _lconfig_alignmentBreakOnMultiline = coerce True
    , _lconfig_hangingTypeSignature      = coerce False
    , _lconfig_reformatModulePreamble    = coerce True
    , _lconfig_allowSingleLineExportList = coerce False
    , _lconfig_allowHangingQuasiQuotes   = coerce True
    -- , _lconfig_allowSinglelineRecord  = coerce False
    }
  , _conf_errorHandling = ErrorHandlingConfig
    { _econf_produceOutputOnErrors   = coerce False
    , _econf_Werror                  = coerce False
    , _econf_ExactPrintFallback      = coerce ExactPrintFallbackModeInline
    , _econf_omit_output_valid_check = coerce False
    }
  , _conf_preprocessor = PreProcessorConfig
    { _ppconf_CPPMode            = coerce CPPModeAbort
    , _ppconf_hackAroundIncludes = coerce False
    }
  , _conf_forward                   = ForwardOptions { _options_ghc = Identity [] }
  , _conf_roundtrip_exactprint_only = coerce False
  , _conf_disable_formatting        = coerce False
  , _conf_obfuscate                 = coerce False
  }

forwardOptionsSyntaxExtsEnabled :: ForwardOptions
forwardOptionsSyntaxExtsEnabled = ForwardOptions
  { _options_ghc = Identity
    [ "-XLambdaCase"
    , "-XMultiWayIf"
    , "-XGADTs"
    , "-XPatternGuards"
    , "-XViewPatterns"
    , "-XTupleSections"
    , "-XExplicitForAll"
    , "-XImplicitParams"
    , "-XQuasiQuotes"
    , "-XTemplateHaskell"
    , "-XBangPatterns"
    , "-XTypeApplications"
    ]
  }

-- brittany-next-binding { lconfig_indentPolicy: IndentPolicyLeft, lconfig_cols: 200 }
cmdlineConfigParser :: Parser (CConfig Maybe)
cmdlineConfigParser = do
  (ind :: Int)         <- option auto $
    long "indent" <>
    metavar "N" <>
    value (coerce (_lconfig_indentAmount (_conf_layout staticDefaultConfig))) <>
    showDefault <>
    help "Spaces per indentation level"

  (cols :: Int)        <- option auto $
    long "columns" <>
    metavar "N" <>
    value (coerce (_lconfig_cols (_conf_layout staticDefaultConfig))) <>
    showDefault <>
    help "Maximum columns"

  (importCol :: Int)   <- option auto $
    long "import-col" <>
    metavar "N" <>
    value (coerce (_lconfig_importColumn (_conf_layout staticDefaultConfig))) <>
    showDefault <>
    help "Column to align import lists at"

  (importAsCol :: Int) <- option auto $
    long "import-as-col" <>
    metavar "N" <>
    value (coerce (_lconfig_importAsColumn (_conf_layout staticDefaultConfig))) <>
    showDefault <>
    help "Column to align qualified-as module names at"

  outputOnErrors       <- switch $
    long "output-on-errors" <>
    help "Even when there are errors, produce output (or try to to the degree possible)"
  wError               <- switch $
   long "werror" <>
   help "Treat warnings as errors"
  omitValidCheck       <- switch $
    long "omit-output-check" <>
    help "Omit checking if the output is syntactically valid (debugging)"

  roundtripOnly        <- switch $
    long "exactprint-only" <>
    help "Do not reformat, but exclusively use exactprint to roundtrip (debugging)"

  optionsGhc           <- many $ strOption $
    long "ghc-option" <>
    metavar "STRING" <>
    help "Allows to define default language extensions. The parameter is forwarded to ghc."
  disableFormatting    <- switch $
    long "disable-formatting" <>
    help "Parse, but don't transform the input at all. Useful for inline config for specific modules."
  obfuscate            <- switch $
    long "obfuscate" <>
    help "Apply obfuscator to the output."

  pure (Config
    { _conf_version = mempty
    , _conf_layout = LayoutConfig
      { _lconfig_cols                      = Just $ Last cols
      , _lconfig_indentPolicy              = mempty
      , _lconfig_indentAmount              = Just $ Last ind
      , _lconfig_indentWhereSpecial        = mempty -- falseToNothing _
      , _lconfig_indentListSpecial         = mempty -- falseToNothing _
      , _lconfig_importColumn              = Just $ Last importCol
      , _lconfig_importAsColumn            = Just $ Last importAsCol
      , _lconfig_altChooser                = mempty
      , _lconfig_columnAlignMode           = mempty
      , _lconfig_alignmentLimit            = mempty
      , _lconfig_alignmentBreakOnMultiline = mempty
      , _lconfig_hangingTypeSignature      = mempty
      , _lconfig_reformatModulePreamble    = mempty
      , _lconfig_allowSingleLineExportList = mempty
      , _lconfig_allowHangingQuasiQuotes   = mempty
      -- , _lconfig_allowSinglelineRecord     = mempty
      } :: CLayoutConfig Maybe
    , _conf_errorHandling = ErrorHandlingConfig
      { _econf_produceOutputOnErrors   = wrapLast $ falseToNothing outputOnErrors
      , _econf_Werror                  = wrapLast $ falseToNothing wError
      , _econf_ExactPrintFallback      = mempty
      , _econf_omit_output_valid_check = wrapLast $ falseToNothing omitValidCheck
      } :: CErrorHandlingConfig Maybe
    , _conf_preprocessor = PreProcessorConfig
      { _ppconf_CPPMode            = mempty
      , _ppconf_hackAroundIncludes = mempty
      } :: CPreProcessorConfig Maybe
    , _conf_forward = ForwardOptions
      { _options_ghc = case optionsGhc of
          [] -> Nothing
          xs -> Just xs
      } :: CForwardOptions Maybe
    , _conf_roundtrip_exactprint_only = wrapLast $ falseToNothing roundtripOnly
    , _conf_disable_formatting        = wrapLast $ falseToNothing disableFormatting
    , _conf_obfuscate                 = wrapLast $ falseToNothing obfuscate
    } :: CConfig Maybe)
  where
    falseToNothing :: Bool -> Maybe Bool
    falseToNothing = Bool.bool Nothing (Just True)
    wrapLast :: Maybe a -> Maybe (Last a)
    wrapLast = coerce

-- | Reads a config from a file. If the file does not exist, returns
-- Nothing. If the file exists and parsing fails, prints to stderr and
-- aborts the MaybeT. Otherwise succeed via Just.
-- If the second parameter is True and the file does not exist, writes the
-- staticDefaultConfig to the file.
readConfig
  :: MonadIO m => FilePath -> MaybeT m (Maybe (CConfig Maybe))
readConfig path = do
  -- TODO: probably should catch IOErrors and then omit the existence check.
  exists <- liftIO $ System.Directory.doesFileExist path
  if exists
    then do
      contents <- liftIO $ ByteString.readFile path -- no lazy IO, tyvm.
      fileConf <- case Data.Yaml.decodeEither' contents of
        Left e -> do
          putStrErrLn
            $ "error reading in brittany config from "
            ++ path
            ++ ":"
          putStrErrLn $ Data.Yaml.prettyPrintParseException e
          mzero
        Right x -> pure x
      pure $ Just fileConf
    else pure $ Nothing

-- | Looks for a user-global config file and pure its path.
-- If there is no global config in a system, one will be created.
userConfigPath :: IO FilePath
userConfigPath = do
  userBritPathSimple <- Directory.getAppUserDataDirectory "brittany"
  userBritPathXdg <- Directory.getXdgDirectory Directory.XdgConfig "brittany"
  let searchDirs = [userBritPathSimple, userBritPathXdg]
  globalConfig <- Directory.findFileWith
    Directory.doesFileExist
    searchDirs
    "config.yaml"
  maybe (writeUserConfig userBritPathXdg) pure globalConfig
 where
  writeUserConfig dir = do
    let createConfPath = dir FilePath.</> "config.yaml"
    liftIO $ Directory.createDirectoryIfMissing True dir
    writeDefaultConfig $ createConfPath
    pure createConfPath

-- | Searches for a local (per-project) brittany config starting from a given directory
findLocalConfigPath :: FilePath -> IO (Maybe FilePath)
findLocalConfigPath dir = do
  let dirParts = FilePath.splitDirectories dir
  -- when provided dir is "a/b/c", searchDirs is ["a/b/c", "a/b", "a", "/"]
  let searchDirs = FilePath.joinPath <$> reverse (L.inits dirParts)
  Directory.findFileWith Directory.doesFileExist searchDirs "brittany.yaml"

-- | Reads specified configs.
readConfigs
  :: CConfig Maybe -- ^ Explicit options, take highest priority
  -> [FilePath]    -- ^ List of config files to load and merge, highest priority first
  -> IO (Maybe (CConfig Identity))
readConfigs cmdlineConfig configPaths = runMaybeT $ do
  configs <- traverse readConfig configPaths
  let merged = sconcat $ NE.reverse (cmdlineConfig :| catMaybes configs)
  pure $ cZipWith fromMaybeIdentity staticDefaultConfig merged

-- | Reads provided configs
-- but also applies the user default configuration (with lowest priority)
readConfigsWithUserConfig
  :: CConfig Maybe -- ^ Explicit options, take highest priority
  -> [FilePath]    -- ^ List of config files to load and merge, highest priority first
  -> IO (Maybe (CConfig Identity))
readConfigsWithUserConfig cmdlineConfig configPaths = do
  defaultPath <- userConfigPath
  readConfigs cmdlineConfig (configPaths ++ [defaultPath])

writeDefaultConfig :: MonadIO m => FilePath -> m ()
writeDefaultConfig path
  = liftIO
  $ C8.writeFile path
  $ Data.Yaml.encode
  $ cMap (Just . runIdentity) staticDefaultConfig

showConfigYaml :: CConfig Identity -> String
showConfigYaml
  = C8.unpack
  . Data.Yaml.encode
  . cMap (\(Identity x) -> Just x)
