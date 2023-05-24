{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Language.Haskell.Brittany.Main
  ( main
  , mainWith
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except qualified as ExceptT
import Data.Either
import Data.Foldable
import Data.List qualified as L
import Data.Maybe
import Data.Semigroup
import Data.Text.IO qualified as TIO
import Data.Version
import Data.Void
import Options.Applicative hiding (str)
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Combinators
import System.Directory qualified as Directory
import System.Environment qualified as Environment
import System.Exit
import System.FilePath.Posix qualified as FilePath

import GHC.Types.SrcLoc (SrcSpan)
import Language.Haskell.Brittany.Internal.Config
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Formatting
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils
import Paths_brittany

data CmdlineConfig = CmdlineConfig
  { cfgLicense      :: Bool
  , cfgVersion      :: Bool
  , cfgNoUserConfig :: Bool
  , cfgConfigFile   :: Maybe FilePath
  , cfgConfig       :: CConfig Maybe
  , cfgInplace      :: Bool
  , cfgCheckMode    :: Bool
  , cfgInputs       :: [FilePath]
  }

optsParser :: Parser CmdlineConfig
optsParser = do
  cfgLicense <- switch $
    long "license" <>
    help "Print license"

  cfgVersion <- switch $
    long "version" <>
    help "Print version"

  cfgNoUserConfig <- switch $
    long "no-user-config"

  cfgConfigFile <- optional $ strOption $
    long "config-file" <>
    metavar "PATH" <>
    help "Path to config file"

  cfgConfig <- cmdlineConfigParser

  cfgInplace <- switch $
    long "inplace" <>
    help "Modify input files in place, without backup"

  cfgCheckMode <- switch $
    long "check-mode" <>
    help "Check for changes but do not write them out. Exits with code 0 if no changes necessary, 1 otherwise and print file path(s) of files that have changes to stdout"

  cfgInputs <- many $ strArgument $
    metavar "PATH" <>
    help "Haskell sources to format"

  pure CmdlineConfig{..}

progInfo :: ParserInfo CmdlineConfig
progInfo = info
  (helper <*> optsParser)
  (fullDesc <> header "haskell source pretty printer" <> progDescDoc (Just descrDoc))

main :: IO ()
main = do
  progName <- Environment.getProgName
  args     <- Environment.getArgs
  mainWith progName args

mainWith :: String -> [String] -> IO ()
mainWith progName args = Environment.withProgName progName $ Environment.withArgs args $
  run =<< customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) progInfo

run :: CmdlineConfig -> IO ()
run CmdlineConfig{cfgLicense, cfgVersion, cfgNoUserConfig, cfgConfigFile, cfgConfig, cfgInplace, cfgCheckMode, cfgInputs} = do
  when cfgLicense $ do
    putDocLn licenseDoc
    exitSuccess
  when cfgVersion $ do
    putDocLn versionDoc
    exitSuccess

  let inputPaths :: [Maybe FilePath]
      inputPaths = case cfgInputs of
        [] -> [Nothing]
        xs -> map Just xs
      outputPaths :: [Maybe FilePath]
      outputPaths = if cfgInplace
        then inputPaths
        else repeat Nothing

  configToLoad <- liftIO $ case cfgConfigFile of
    Nothing -> Directory.getCurrentDirectory >>= findLocalConfigPath
    Just x  -> pure $ Just x

  config <- (maybe (exitWith (ExitFailure 53)) pure =<<) $
    if cfgNoUserConfig
    then readConfigs cfgConfig $ maybeToList configToLoad
    else readConfigsWithUserConfig cfgConfig $ maybeToList configToLoad

  results <- zipWithM
    (formatModule config cfgCheckMode)
    inputPaths
    outputPaths

  if cfgCheckMode
  then when (Changes `elem` rights results)
    $ exitWith (ExitFailure 1)
  else case results of
    xs | all isRight xs -> pure ()
    [Left x]            -> exitWith (ExitFailure x)
    _                   -> exitWith (ExitFailure 1)

descrDoc :: PP.Doc ann
descrDoc = PP.vcat $ L.intersperse mempty
  [ parDocW
    [ "Reformats one or more haskell modules."
    , "Currently affects only the module head (imports/exports), type"
    , "signatures and function bindings;"
    , "everything else is left unmodified."
    , "Based on ghc-exactprint, thus (theoretically) supporting all"
    , "that ghc does."
    ]
  , parDoc "Example invocations:"
  , PP.hang 2 $ PP.vcat
    [ "brittany"
    , PP.nest 2 $ "read from stdin, output to stdout"
    ]
  , PP.hang 2 $ PP.vcat
    [ "brittany --indent=4 --inplace *.hs"
    , PP.nest 2 $ PP.vcat
      [ "run on all modules in current directory (no backup!)"
      , "4 spaces indentation"
      ]
    ]
  , parDocW
    [ "This program is written carefully and contains safeguards to ensure"
    , "the output is syntactically valid and that no comments are removed."
    , "Nonetheless, this is a young project, and there will always be bugs,"
    , "and ensuring that the transformation never changes semantics of the"
    , "transformed source is currently not possible."
    , "Please do check the output and do not let brittany override your large"
    , "codebase without having backups."
    ]
  , parDoc "There is NO WARRANTY, to the extent permitted by law."
  , parDocW
    [ "This program is free software released under the AGPLv3."
    , "For details use the --license flag."
    ]
  , "See https://github.com/lspitzner/brittany"
  , "Please report bugs at https://github.com/lspitzner/brittany/issues"
  ]

licenseDoc :: PP.Doc ann
licenseDoc = PP.vcat $ L.intersperse mempty
  [ "Copyright (C) 2016-2019 Lennart Spitzner\n\
    \Copyright (C) 2019 PRODA LTD\n\
    \Copyright (C) 2022-2023 Sergey Vinokurov"
  , parDocW
    [ "This program is free software: you can redistribute it and/or modify"
    , "it under the terms of the GNU Affero General Public License,"
    , "version 3, as published by the Free Software Foundation."
    ]
  , parDocW
    [ "This program is distributed in the hope that it will be useful,"
    , "but WITHOUT ANY WARRANTY; without even the implied warranty of"
    , "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
    , "GNU Affero General Public License for more details."
    ]
  , parDocW
    [ "You should have received a copy of the GNU Affero General Public"
    , "License along with this program.  If not, see"
    , "<http://www.gnu.org/licenses/>."
    ]
  ]

versionDoc :: PP.Doc ann
versionDoc = PP.vcat
  [ "brittany version" <+> pretty (showVersion version)
  , "Copyright (C) 2016-2019 Lennart Spitzner"
  , "Copyright (C) 2019 PRODA LTD"
  , "Copyright (C) 2022-2023 Sergey Vinokurov"
  , "There is NO WARRANTY, to the extent permitted by law."
  ]

data ChangeStatus = Changes | NoChanges
  deriving (Eq)

classifyError
  :: BrittanyError
  -> ([(String, String)], [String], [(String, SrcSpan, Doc Void)], [String])
classifyError = \case
  ErrorMacroConfig x y         -> ([(x, y)], mempty, mempty,            mempty)
  LayoutWarning msg            -> (mempty,   [msg],  mempty,            mempty)
  ErrorUnknownNode msg loc ast -> (mempty,   mempty, [(msg, loc, ast)], mempty)
  ErrorOutputCheck msg         -> (mempty,   mempty, mempty,            [msg])

-- | The main IO parts for the default mode of operation, and after commandline
-- and config stuff is processed.
formatModule
  :: Config                       -- ^ global program config.
  -> Bool                         -- ^ whether we are (just) in check mode.
  -> Maybe FilePath.FilePath      -- ^ input filepath; stdin if Nothing.
  -> Maybe FilePath.FilePath      -- ^ output filepath; stdout if Nothing.
  -> IO (Either Int ChangeStatus) -- ^ Either an errorNo, or the change status.
formatModule config cfgCheckMode inputPathM outputPathM =
  ExceptT.runExceptT $ do

    (input, fname) <- liftIO $
      case inputPathM of
        Nothing    -> (, "stdin") <$> TIO.getContents
        Just fname -> (, fname)   <$> TIO.readFile fname
    res <- liftIO $ format config fname input

    (formatted, warns, errs, logs, hasChanges) <- case res of
      Left (ParseError err) -> do
        putStrErrLn "parse error:"
        putStrErrLn err
        ExceptT.throwE 60
      Right x -> pure x

    unless (null logs) $
      traverse_ putStrErrLn logs

    unless (null warns) $ do
      when (any (== CPPWarning) warns)
        $ putStrErrLn
        $ "Warning: Encountered -XCPP."
        ++ " Be warned that -XCPP is not supported and that"
        ++ " brittany cannot check that its output is syntactically"
        ++ " valid in its presence."

    unless (null errs) $ do
      let (macroConfs, layoutWarns, unknownNodes, outputChecks) = foldMap classifyError errs

      unless (null unknownNodes) $ do
        putStrErrLn "WARNING: encountered unknown syntactical constructs:"
        for_ unknownNodes $ \(str, loc, ast) -> do
          putStrErrLn $ renderString $ "  " <> pretty str <+> "at" <+> pretty loc <> ":" ## ast
        putStrErrLn
          "  -> falling back on exactprint for this element of the module"

      unless (null layoutWarns) $ do
        putStrErrLn "WARNINGS:"
        traverse_ putStrErrLn layoutWarns

      unless (null outputChecks) $ do
        putStrErrLn "ERROR: brittany pretty printer returned syntactically invalid results:"
        traverse_ putStrErrLn outputChecks

      unless (null macroConfs) $ do
        putStrErrLn "Error: parse error in inline configuration:"
        for_ macroConfs $ \(err, str) -> do
          putStrErrLn err
          putStrErrLn $ "  in the string \"" ++ str ++ "\"."

    let hasErrors    =
          if confUnpack (_econf_Werror (_conf_errorHandling config))
          then not $ null errs
          else flip any errs $ \case
            ErrorMacroConfig{} -> True
            ErrorOutputCheck{} -> True
            LayoutWarning{}    -> False
            ErrorUnknownNode{} -> False
        outputOnErrs :: Bool
        outputOnErrs = confUnpack (_econf_produceOutputOnErrors (_conf_errorHandling config))
        shouldOutput :: Bool
        shouldOutput = not cfgCheckMode && (not hasErrors || outputOnErrs)

    liftIO $ when shouldOutput $
      case outputPathM of
        Nothing -> TIO.putStr formatted
        Just p  -> do
          let isIdentical = case inputPathM of
                Nothing -> False
                Just _  -> not hasChanges
          unless isIdentical $ TIO.writeFile p formatted

    when (cfgCheckMode && hasChanges) $ case inputPathM of
      Nothing -> pure ()
      Just p  -> liftIO $ putStrLn $ "formatting would modify: " ++ p

    when hasErrors $ ExceptT.throwE 70
    pure (if hasChanges then Changes else NoChanges)
