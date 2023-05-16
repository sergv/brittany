{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TupleSections     #-}

module BrittanyTestSuite (main) where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Coerce
import Data.Foldable
import Data.Functor.Identity
import Data.List qualified as L
import Data.Maybe
import Data.Semigroup
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Traversable
import Options.Applicative
import System.Directory
import System.Environment (getArgs, withArgs)
import System.Exit (die)
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Text.Read

import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Formatting qualified as Brittany

import Language.Haskell.Brittany.Internal.Layouters.Module.Tests qualified

data TestsConfig = TestsConfig
  { tcfgInputDirs :: ![FilePath]
  }

optsParser :: Parser TestsConfig
optsParser = do
  tcfgInputDirs <- many $ strArgument $
    metavar "DIR" <>
    help "Directory with Haskell files to format and use as test cases"
  pure TestsConfig{tcfgInputDirs}

progInfo :: ParserInfo TestsConfig
progInfo = info
  (helper <*> optsParser)
  (fullDesc <> header "Test suite for the brittany formatter")

main :: IO ()
main = do
  (ourArgs, tastyArgs) <- second (drop 1) . break (== "--") <$> getArgs
  TestsConfig{tcfgInputDirs} <- handleParseResult $
    execParserPure (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) progInfo ourArgs

  let dirs = case tcfgInputDirs of
        [] -> ["data"]
        xs -> xs

  for_ dirs $ \dir -> do
    exists <- doesDirectoryExist dir
    unless exists $
      die $ "Input directory does not exist: " ++ dir

  withArgs tastyArgs . defaultMain . testGroup "All tests" . (unitTests :) . (: []) =<< makeTests dirs

testConfig :: Config
testConfig = Config
  { _conf_version = coerce (1 :: Int)
  , _conf_debug = DebugConfig
    { _dconf_dump_config                = coerce False
    , _dconf_dump_annotations           = coerce False
    , _dconf_dump_ast_unknown           = coerce False
    , _dconf_dump_ast_full              = coerce False
    , _dconf_dump_bridoc_raw            = coerce False
    , _dconf_dump_bridoc_simpl_alt      = coerce False
    , _dconf_dump_bridoc_simpl_floating = coerce False
    , _dconf_dump_bridoc_simpl_par      = coerce False
    , _dconf_dump_bridoc_simpl_columns  = coerce False
    , _dconf_dump_bridoc_simpl_indent   = coerce False
    , _dconf_dump_bridoc_final          = coerce False
    , _dconf_roundtrip_exactprint_only  = coerce False
    }
  , _conf_layout = LayoutConfig
    { _lconfig_cols                      = coerce (80 :: Int)
    , _lconfig_indentPolicy              = coerce IndentPolicyFree
    , _lconfig_indentAmount              = coerce (2  :: Int)
    , _lconfig_indentWhereSpecial        = coerce True
    , _lconfig_indentListSpecial         = coerce True
    , _lconfig_importColumn              = coerce (60 :: Int)
    , _lconfig_importAsColumn            = coerce (60 :: Int)
    -- , _lconfig_importColumn              = coerce (50 :: Int)
    -- , _lconfig_importAsColumn            = coerce (50 :: Int)
    , _lconfig_altChooser                = coerce (AltChooserBoundedSearch 3)
    , _lconfig_columnAlignMode           = coerce (ColumnAlignModeMajority 0.7)
    , _lconfig_alignmentLimit            = coerce (30 :: Int)
    , _lconfig_alignmentBreakOnMultiline = coerce True
    , _lconfig_hangingTypeSignature      = coerce False
    , _lconfig_reformatModulePreamble    = coerce True
    , _lconfig_allowSingleLineExportList = coerce True
    -- , _lconfig_allowSingleLineExportList = coerce False
    , _lconfig_allowHangingQuasiQuotes   = coerce True
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

makeTests :: [FilePath] -> IO TestTree
makeTests dirs = do
  groups <- for dirs $ \dir -> do
    files <- mapMaybe (\x -> (, x) <$> stripExtension "hs" x) <$> listDirectory dir
    pure $ testGroup dir $ flip map (sortTestInputs files) $ \(stem, file) ->
      testCase stem $ do
        input <- TIO.readFile $ dir </> file
        res   <- Brittany.format testConfig file input
        case res of
          Left (Brittany.ParseError err) ->
            assertFailure $ "Failed to parse test module:\n" ++ err
          Right (formatted, _warns, _errs, _logs, _isChanged) ->
            unless (input == formatted) $ assertFailure $
              "expected: "  ++ show input         ++ "\n" ++
              "got:      "  ++ show formatted     ++ "\n" ++
              "================================================================\n" ++
              "expected:\n" ++ T.unpack input     ++ "\n" ++
              "----------------------------------------------------------------\n" ++
              "got:\n"      ++ T.unpack formatted ++ "\n" ++
              "----------------------------------------------------------------\n"

  pure $ testGroup "End to end tests" groups

sortTestInputs :: [(FilePath, a)] -> [(FilePath, a)]
sortTestInputs = L.sortOn (extractTestNum . fst)
  where
    extractTestNum :: String -> (Maybe Int, String)
    extractTestNum x = case L.stripPrefix "Test" x of
      Nothing   -> (Nothing, x)
      Just rest -> (readMaybe $ takeWhile isDigit rest, x)

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ Language.Haskell.Brittany.Internal.Layouters.Module.Tests.tests
  ]

