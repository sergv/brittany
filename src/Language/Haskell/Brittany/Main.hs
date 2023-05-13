{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Language.Haskell.Brittany.Main (main, mainWith) where

import Control.Monad (zipWithM)
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Data.Either
import Data.Foldable
import qualified Data.Monoid
import qualified Data.Semigroup as Semigroup
import qualified Data.Text.IO as TIO
import GHC (GenLocated(L), SrcSpan)
import qualified GHC.OldList as List
import GHC.Parser.Annotation (SrcSpanAnn'(locA))
import GHC.Utils.Outputable (Outputable(..), showSDocUnsafe)
import Language.Haskell.Brittany.Internal.Config
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Formatting
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils
import Paths_brittany
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit
import qualified System.FilePath.Posix as FilePath
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.PrettyPrint as PP
import Text.Read (Read(..))
import UI.Butcher.Monadic

data WriteMode = Display | Inplace

instance Read WriteMode where
  readPrec = val "display" Display <|> val "inplace" Inplace
    where val iden v = ReadPrec.lift $ ReadP.string iden >> return v

instance Show WriteMode where
  show Display = "display"
  show Inplace = "inplace"

main :: IO ()
main = do
  progName <- Environment.getProgName
  args <- Environment.getArgs
  mainWith progName args

mainWith :: String -> [String] -> IO ()
mainWith progName args =
  Environment.withProgName progName
    . Environment.withArgs args
    $ mainFromCmdParserWithHelpDesc mainCmdParser

helpDoc :: PP.Doc
helpDoc = PP.vcat $ List.intersperse
  (PP.text "")
  [ parDocW
    [ "Reformats one or more haskell modules."
    , "Currently affects only the module head (imports/exports), type"
    , "signatures and function bindings;"
    , "everything else is left unmodified."
    , "Based on ghc-exactprint, thus (theoretically) supporting all"
    , "that ghc does."
    ]
  , parDoc $ "Example invocations:"
  , PP.hang (PP.text "") 2 $ PP.vcat
    [ PP.text "brittany"
    , PP.nest 2 $ PP.text "read from stdin, output to stdout"
    ]
  , PP.hang (PP.text "") 2 $ PP.vcat
    [ PP.text "brittany --indent=4 --write-mode=inplace *.hs"
    , PP.nest 2 $ PP.vcat
      [ PP.text "run on all modules in current directory (no backup!)"
      , PP.text "4 spaces indentation"
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
  , parDoc $ "There is NO WARRANTY, to the extent permitted by law."
  , parDocW
    [ "This program is free software released under the AGPLv3."
    , "For details use the --license flag."
    ]
  , parDoc $ "See https://github.com/lspitzner/brittany"
  , parDoc
  $ "Please report bugs at"
  ++ " https://github.com/lspitzner/brittany/issues"
  ]

licenseDoc :: PP.Doc
licenseDoc = PP.vcat $ List.intersperse
  (PP.text "")
  [ parDoc $ "Copyright (C) 2016-2019 Lennart Spitzner"
  , parDoc $ "Copyright (C) 2019 PRODA LTD"
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


mainCmdParser :: CommandDesc () -> CmdParser Identity (IO ()) ()
mainCmdParser helpDesc = do
  addCmdSynopsis "haskell source pretty printer"
  addCmdHelp $ helpDoc
  -- addCmd "debugArgs" $ do
  addHelpCommand helpDesc
  addCmd "license" $ addCmdImpl $ print $ licenseDoc
  -- addButcherDebugCommand
  reorderStart
  printHelp <- addSimpleBoolFlag "h" ["help"] mempty
  printVersion <- addSimpleBoolFlag "" ["version"] mempty
  printLicense <- addSimpleBoolFlag "" ["license"] mempty
  noUserConfig <- addSimpleBoolFlag "" ["no-user-config"] mempty
  configPaths <- addFlagStringParams
    ""
    ["config-file"]
    "PATH"
    (flagHelpStr "path to config file") -- TODO: allow default on addFlagStringParam ?
  cmdlineConfig <- cmdlineConfigParser
  suppressOutput <- addSimpleBoolFlag
    ""
    ["suppress-output"]
    (flagHelp $ parDoc
      "suppress the regular output, i.e. the transformed haskell source"
    )
  _verbosity <- addSimpleCountFlag
    "v"
    ["verbose"]
    (flagHelp $ parDoc "[currently without effect; TODO]")
  checkMode <- addSimpleBoolFlag
    "c"
    ["check-mode"]
    (flagHelp
      (PP.vcat
        [ PP.text "check for changes but do not write them out"
        , PP.text "exits with code 0 if no changes necessary, 1 otherwise"
        , PP.text "and print file path(s) of files that have changes to stdout"
        ]
      )
    )
  writeMode <- addFlagReadParam
    ""
    ["write-mode"]
    "(display|inplace)"
    (flagHelp
        (PP.vcat
          [ PP.text "display: output for any input(s) goes to stdout"
          , PP.text "inplace: override respective input file (without backup!)"
          ]
        )
    Data.Monoid.<> flagDefault Display
    )
  inputParams <- addParamNoFlagStrings
    "PATH"
    (paramHelpStr "paths to input/inout haskell source files")
  reorderStop
  addCmdImpl $ void $ do
    when printLicense $ do
      print licenseDoc
      System.Exit.exitSuccess
    when printVersion $ do
      do
        putStrLn $ "brittany version " ++ showVersion version
        putStrLn $ "Copyright (C) 2016-2019 Lennart Spitzner"
        putStrLn $ "Copyright (C) 2019 PRODA LTD"
        putStrLn $ "Copyright (C) 2022 Sergey Vinokurov"
        putStrLn $ "There is NO WARRANTY, to the extent permitted by law."
      System.Exit.exitSuccess
    when printHelp $ do
      liftIO
        $ putStrLn
        $ PP.renderStyle PP.style { PP.ribbonsPerLine = 1.0 }
        $ ppHelpShallow helpDesc
      System.Exit.exitSuccess

    let
      inputPaths = if null inputParams then [Nothing] else map Just inputParams
    let
      outputPaths = case writeMode of
        Display -> repeat Nothing
        Inplace -> inputPaths

    configsToLoad <- liftIO $ if null configPaths
      then
        maybeToList <$> (Directory.getCurrentDirectory >>= findLocalConfigPath)
      else pure configPaths

    config <-
      runMaybeT
          (if noUserConfig
            then readConfigs cmdlineConfig configsToLoad
            else readConfigsWithUserConfig cmdlineConfig configsToLoad
          )
        >>= \case
              Nothing -> System.Exit.exitWith (System.Exit.ExitFailure 53)
              Just x -> return x
    when (config & _conf_debug & _dconf_dump_config & confUnpack)
      $ trace (showConfigYaml config)
      $ return ()

    results <- zipWithM
      (coreIO config suppressOutput checkMode)
      inputPaths
      outputPaths

    if checkMode
      then when (Changes `elem` (Data.Either.rights results))
        $ System.Exit.exitWith (System.Exit.ExitFailure 1)
      else case results of
        xs | all Data.Either.isRight xs -> pure ()
        [Left x] -> System.Exit.exitWith (System.Exit.ExitFailure x)
        _ -> System.Exit.exitWith (System.Exit.ExitFailure 1)


data ChangeStatus = Changes | NoChanges
  deriving (Eq)

classifyError
  :: BrittanyError
  -> ([(String, String)], [String], [(String, SrcSpan, PP.Doc)], [String])
classifyError = \case
  ErrorMacroConfig x y             -> ([(x, y)], mempty, mempty, mempty)
  LayoutWarning msg                -> (mempty, [msg], mempty, mempty)
  ErrorUnknownNode msg (L loc ast) -> (mempty, mempty, [(msg, locA loc, astToDoc ast)] , mempty)
  ErrorOutputCheck msg             -> (mempty, mempty, mempty, [msg])

-- | The main IO parts for the default mode of operation, and after commandline
-- and config stuff is processed.
coreIO
  :: Config                       -- ^ global program config.
  -> Bool                         -- ^ whether to supress output (to stdout). Purely IO flag, so
                                  -- currently not part of program config.
  -> Bool                         -- ^ whether we are (just) in check mode.
  -> Maybe FilePath.FilePath      -- ^ input filepath; stdin if Nothing.
  -> Maybe FilePath.FilePath      -- ^ output filepath; stdout if Nothing.
  -> IO (Either Int ChangeStatus) -- ^ Either an errorNo, or the change status.
coreIO config suppressOutput checkMode inputPathM outputPathM =
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
      let Any isCppWarning = foldMap (Any . (== CPPWarning)) warns

      when isCppWarning
        $ putStrErrLn
        $ "Warning: Encountered -XCPP."
        ++ " Be warned that -XCPP is not supported and that"
        ++ " brittany cannot check that its output is syntactically"
        ++ " valid in its presence."

    unless (null errs) $ do
      let (macroConfs, layoutWarns, unknownNodes, outputChecks) = foldMap classifyError errs

      unless (null unknownNodes) $ do
        putStrErrLn "WARNING: encountered unknown syntactical constructs:"
        for_ unknownNodes $ \(str, loc, astDoc) -> do
          putStrErrLn $ "  " <> str <> " at " <> showSDocUnsafe (ppr loc)
          when (confUnpack (_dconf_dump_ast_unknown (_conf_debug config))) $
            putStrErrLn $ "  " ++ show astDoc
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
        shouldOutput = not suppressOutput && not checkMode && (not hasErrors || outputOnErrs)

    when shouldOutput
      $ addTraceSep (_conf_debug config)
      $ case outputPathM of
          Nothing -> liftIO $ TIO.putStr formatted
          Just p -> liftIO $ do
            let
              isIdentical = case inputPathM of
                Nothing -> False
                Just _ -> not hasChanges
            unless isIdentical $ TIO.writeFile p formatted

    when (checkMode && hasChanges) $ case inputPathM of
      Nothing -> pure ()
      Just p  -> liftIO $ putStrLn $ "formatting would modify: " ++ p

    when hasErrors $ ExceptT.throwE 70
    pure (if hasChanges then Changes else NoChanges)
  where
  addTraceSep conf =
    if or
        [ confUnpack $ _dconf_dump_annotations conf
        , confUnpack $ _dconf_dump_ast_unknown conf
        , confUnpack $ _dconf_dump_ast_full conf
        , confUnpack $ _dconf_dump_bridoc_raw conf
        , confUnpack $ _dconf_dump_bridoc_simpl_alt conf
        , confUnpack $ _dconf_dump_bridoc_simpl_floating conf
        , confUnpack $ _dconf_dump_bridoc_simpl_columns conf
        , confUnpack $ _dconf_dump_bridoc_simpl_indent conf
        , confUnpack $ _dconf_dump_bridoc_final conf
        ]
      then trace "----"
      else id
