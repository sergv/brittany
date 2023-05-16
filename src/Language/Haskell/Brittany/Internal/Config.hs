{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

import Data.Bool qualified as Bool
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified
import Data.CZipWith
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup qualified as Semigroup
import Data.Yaml qualified
import GHC.OldList qualified as List
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Config.Types.Instances ()
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Utils
import System.Console.CmdArgs.Explicit qualified as CmdArgs
import System.Directory qualified
import System.Directory qualified as Directory
import System.FilePath.Posix qualified as FilePath
import System.IO qualified
import UI.Butcher.Monadic

-- brittany-next-binding { lconfig_indentPolicy: IndentPolicyLeft }
staticDefaultConfig :: Config
staticDefaultConfig = Config
  { _conf_version = coerce (1 :: Int)
  , _conf_layout = LayoutConfig
    { _lconfig_cols = coerce (80 :: Int)
    , _lconfig_indentPolicy = coerce IndentPolicyFree
    , _lconfig_indentAmount = coerce (2 :: Int)
    , _lconfig_indentWhereSpecial = coerce True
    , _lconfig_indentListSpecial = coerce True
    , _lconfig_importColumn = coerce (50 :: Int)
    , _lconfig_importAsColumn = coerce (50 :: Int)
    , _lconfig_altChooser = coerce (AltChooserBoundedSearch 3)
    , _lconfig_columnAlignMode = coerce (ColumnAlignModeMajority 0.7)
    , _lconfig_alignmentLimit = coerce (30 :: Int)
    , _lconfig_alignmentBreakOnMultiline = coerce True
    , _lconfig_hangingTypeSignature = coerce False
    , _lconfig_reformatModulePreamble = coerce True
    , _lconfig_allowSingleLineExportList = coerce False
    , _lconfig_allowHangingQuasiQuotes = coerce True
    -- , _lconfig_allowSinglelineRecord     = coerce False
    }
  , _conf_errorHandling = ErrorHandlingConfig
    { _econf_produceOutputOnErrors = coerce False
    , _econf_Werror = coerce False
    , _econf_ExactPrintFallback = coerce ExactPrintFallbackModeInline
    , _econf_omit_output_valid_check = coerce False
    }
  , _conf_preprocessor = PreProcessorConfig
    { _ppconf_CPPMode = coerce CPPModeAbort
    , _ppconf_hackAroundIncludes = coerce False
    }
  , _conf_forward = ForwardOptions { _options_ghc = Identity [] }
  , _conf_roundtrip_exactprint_only = coerce False
  , _conf_disable_formatting = coerce False
  , _conf_obfuscate = coerce False
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
cmdlineConfigParser :: CmdParser Identity out (CConfig Maybe)
cmdlineConfigParser = do
  -- TODO: why does the default not trigger; ind never should be []!!
  ind <- addFlagReadParams "" ["indent"] "AMOUNT" (flagHelpStr "spaces per indentation level")
  cols <- addFlagReadParams "" ["columns"] "AMOUNT" (flagHelpStr "target max columns (80 is an old default for this)")
  importCol <- addFlagReadParams "" ["import-col"] "N" (flagHelpStr "column to align import lists at")
  importAsCol <- addFlagReadParams "" ["import-as-col"] "N" (flagHelpStr "column to qualified-as module names at")

  outputOnErrors <- addSimpleBoolFlag "" ["output-on-errors"] (flagHelp $ parDoc "even when there are errors, produce output (or try to to the degree possible)")
  wError <- addSimpleBoolFlag "" ["werror"] (flagHelp $ parDoc "treat warnings as errors")
  omitValidCheck <- addSimpleBoolFlag "" ["omit-output-check"] (flagHelp $ parDoc "omit checking if the output is syntactically valid (debugging)")

  roundtripOnly <- addSimpleBoolFlag "" ["exactprint-only"] (flagHelp $ parDoc "do not reformat, but exclusively use exactprint to roundtrip (debugging)")

  optionsGhc <- addFlagStringParams "" ["ghc-options"] "STRING" (flagHelp $ parDoc "allows to define default language extensions. The parameter is forwarded to ghc.")
  disableFormatting <- addSimpleBoolFlag "" ["disable-formatting"] (flagHelp $ parDoc "parse, but don't transform the input at all. Useful for inline config for specific modules.")
  obfuscate <- addSimpleBoolFlag "" ["obfuscate"] (flagHelp $ parDoc "apply obfuscator to the output.")

  pure $ Config
    { _conf_version = mempty
    , _conf_layout = LayoutConfig
      { _lconfig_cols = optionConcat cols
      , _lconfig_indentPolicy = mempty
      , _lconfig_indentAmount = optionConcat ind
      , _lconfig_indentWhereSpecial = mempty -- falseToNothing _
      , _lconfig_indentListSpecial = mempty -- falseToNothing _
      , _lconfig_importColumn = optionConcat importCol
      , _lconfig_importAsColumn = optionConcat importAsCol
      , _lconfig_altChooser = mempty
      , _lconfig_columnAlignMode = mempty
      , _lconfig_alignmentLimit = mempty
      , _lconfig_alignmentBreakOnMultiline = mempty
      , _lconfig_hangingTypeSignature = mempty
      , _lconfig_reformatModulePreamble = mempty
      , _lconfig_allowSingleLineExportList = mempty
      , _lconfig_allowHangingQuasiQuotes = mempty
      -- , _lconfig_allowSinglelineRecord     = mempty
      }
    , _conf_errorHandling = ErrorHandlingConfig
      { _econf_produceOutputOnErrors = wrapLast $ falseToNothing outputOnErrors
      , _econf_Werror = wrapLast $ falseToNothing wError
      , _econf_ExactPrintFallback = mempty
      , _econf_omit_output_valid_check = wrapLast $ falseToNothing omitValidCheck
      }
    , _conf_preprocessor = PreProcessorConfig { _ppconf_CPPMode = mempty, _ppconf_hackAroundIncludes = mempty }
    , _conf_forward = ForwardOptions { _options_ghc = [ optionsGhc & List.unwords & CmdArgs.splitArgs | not $ null optionsGhc ] }
    , _conf_roundtrip_exactprint_only = wrapLast $ falseToNothing roundtripOnly
    , _conf_disable_formatting = wrapLast $ falseToNothing disableFormatting
    , _conf_obfuscate = wrapLast $ falseToNothing obfuscate
    }
 where
  falseToNothing = Bool.bool Nothing (Just True)
  wrapLast :: Maybe a -> Maybe (Semigroup.Last a)
  wrapLast = fmap Semigroup.Last
  optionConcat :: (Semigroup.Semigroup (f a), Applicative f) => [a] -> Maybe (f a)
  optionConcat = mconcat . fmap (pure . pure)

-- | Reads a config from a file. If the file does not exist, returns
-- Nothing. If the file exists and parsing fails, prints to stderr and
-- aborts the MaybeT. Otherwise succeed via Just.
-- If the second parameter is True and the file does not exist, writes the
-- staticDefaultConfig to the file.
readConfig
  :: MonadIO m => System.IO.FilePath -> MaybeT m (Maybe (CConfig Maybe))
readConfig path = do
  -- TODO: probably should catch IOErrors and then omit the existence check.
  exists <- liftIO $ System.Directory.doesFileExist path
  if exists
    then do
      contents <- liftIO $ ByteString.readFile path -- no lazy IO, tyvm.
      fileConf <- case Data.Yaml.decodeEither' contents of
        Left e -> do
          liftIO
            $ putStrErrLn
            $ "error reading in brittany config from "
            ++ path
            ++ ":"
          liftIO $ putStrErrLn (Data.Yaml.prettyPrintParseException e)
          mzero
        Right x -> pure x
      pure $ Just fileConf
    else pure $ Nothing

-- | Looks for a user-global config file and pure its path.
-- If there is no global config in a system, one will be created.
userConfigPath :: IO System.IO.FilePath
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
findLocalConfigPath :: System.IO.FilePath -> IO (Maybe System.IO.FilePath)
findLocalConfigPath dir = do
  let dirParts = FilePath.splitDirectories dir
  -- when provided dir is "a/b/c", searchDirs is ["a/b/c", "a/b", "a", "/"]
  let searchDirs = FilePath.joinPath <$> reverse (List.inits dirParts)
  Directory.findFileWith Directory.doesFileExist searchDirs "brittany.yaml"

-- | Reads specified configs.
readConfigs
  :: CConfig Maybe        -- ^ Explicit options, take highest priority
  -> [System.IO.FilePath]  -- ^ List of config files to load and merge, highest priority first
  -> MaybeT IO Config
readConfigs cmdlineConfig configPaths = do
  configs <- readConfig `mapM` configPaths
  let
    merged =
      Semigroup.sconcat $ NonEmpty.reverse (cmdlineConfig :| catMaybes configs)
  pure $ cZipWith fromOptionIdentity staticDefaultConfig merged

-- | Reads provided configs
-- but also applies the user default configuration (with lowest priority)
readConfigsWithUserConfig
  :: CConfig Maybe        -- ^ Explicit options, take highest priority
  -> [System.IO.FilePath]  -- ^ List of config files to load and merge, highest priority first
  -> MaybeT IO Config
readConfigsWithUserConfig cmdlineConfig configPaths = do
  defaultPath <- liftIO $ userConfigPath
  readConfigs cmdlineConfig (configPaths ++ [defaultPath])

writeDefaultConfig :: MonadIO m => System.IO.FilePath -> m ()
writeDefaultConfig path =
  liftIO $ ByteString.writeFile path $ Data.Yaml.encode $ cMap
    (Just . runIdentity)
    staticDefaultConfig

showConfigYaml :: Config -> String
showConfigYaml = Data.ByteString.Char8.unpack . Data.Yaml.encode . cMap
  (\(Identity x) -> Just x)
