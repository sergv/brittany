{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Brittany.Main (main, mainWith) where

import Control.Monad (zipWithM)
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Data.Either
import qualified Data.List.Extra
import qualified Data.Monoid
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import DataTreePrint
import GHC (GenLocated(L))
import qualified GHC.Driver.Session as GHC
import qualified GHC.LanguageExtensions.Type as GHC
import qualified GHC.OldList as List
import GHC.Utils.Outputable (Outputable(..), showSDocUnsafe)
import Language.Haskell.Brittany.Internal
import Language.Haskell.Brittany.Internal.Config
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Obfuscation
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
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

cppCheckFunc :: MonadIO m => CPPMode -> GHC.DynFlags -> m (Either String Bool)
cppCheckFunc cppMode dynFlags = if GHC.xopt GHC.Cpp dynFlags
  then case cppMode of
    CPPModeAbort -> do
      return $ Left "Encountered -XCPP. Aborting."
    CPPModeWarn -> do
      putStrErrLn
        $ "Warning: Encountered -XCPP."
        ++ " Be warned that -XCPP is not supported and that"
        ++ " brittany cannot check that its output is syntactically"
        ++ " valid in its presence."
      return $ Right True
    CPPModeNowarn -> return $ Right True
  else return $ Right False

isPreprocessorLine :: Text -> Bool
isPreprocessorLine s = case T.uncons s of
  Just ('#', rest) -> "include" `T.isPrefixOf` T.stripStart rest
  _                -> False

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
    let ghcOptions = config & _conf_forward & _options_ghc & runIdentity
    -- there is a good of code duplication between the following code and the
    -- `pureModuleTransform` function. Unfortunately, there are also a good
    -- amount of slight differences: This module is a bit more verbose, and
    -- it tries to use the full-blown `parseModule` function which supports
    -- CPP (but requires the input to be a file..).
    let cppMode = config & _conf_preprocessor & _ppconf_CPPMode & confUnpack
    let
      hackAroundIncludes =
        config & _conf_preprocessor & _ppconf_hackAroundIncludes & confUnpack
    let
      exactprintOnly = viaGlobal || viaDebug
       where
        viaGlobal = config & _conf_roundtrip_exactprint_only & confUnpack
        viaDebug =
          config & _conf_debug & _dconf_roundtrip_exactprint_only & confUnpack

    (parseResult, originalContents) <- liftIO $ case inputPathM of
      Nothing -> do
        let hackF s = if isPreprocessorLine s
              then "-- BRITANY_INCLUDE_HACK " <> s
              else s
        let hackTransform = if hackAroundIncludes && not exactprintOnly
              then T.intercalate "\n" . fmap hackF . lines'
              else id
        input    <- TIO.getContents
        let input' = T.unpack (hackTransform input)
        parseRes <- parseModuleFromString ghcOptions "stdin" (cppCheckFunc cppMode) input'
        return (parseRes, input)
      Just fname -> do
        input    <- TIO.readFile fname
        parseRes <- parseModuleFromString ghcOptions fname (cppCheckFunc cppMode) (T.unpack input)
        return (parseRes, input)

    case parseResult of
      Left left -> do
        putStrErrLn "parse error:"
        putStrErrLn left
        ExceptT.throwE 60
      Right (anns, parsedSource, hasCPP) -> do
        let moduleConf = config
        when (config & _conf_debug & _dconf_dump_ast_full & confUnpack) $ do
          let val = printTreeWithCustom 100 (customLayouterF anns) parsedSource
          trace ("---- ast ----\n" ++ show val) $ return ()
        let
          disableFormatting =
            moduleConf & _conf_disable_formatting & confUnpack
        (errsWarns, outSText, hasChanges) <- do
          if
            | disableFormatting -> do
              pure ([], originalContents, False)
            | exactprintOnly -> do
              let r = T.pack $ ExactPrint.exactPrint parsedSource anns
              pure ([], r, r /= originalContents)
            | otherwise -> do
              let
                omitCheck =
                  moduleConf
                    & _conf_errorHandling
                    .> _econf_omit_output_valid_check
                    .> confUnpack
              (ews, outRaw) <- if hasCPP || omitCheck
                then pure $ pPrintModule moduleConf anns parsedSource
                else liftIO $ pPrintModuleAndCheck moduleConf anns parsedSource
              let hackF s = fromMaybe s $ TL.stripPrefix "-- BRITANY_INCLUDE_HACK " s
              let
                out = TL.toStrict $ if hackAroundIncludes
                  then TL.intercalate "\n" $ hackF <$> TL.splitOn "\n" outRaw
                  else outRaw
              out' <- if moduleConf & _conf_obfuscate & confUnpack
                then lift $ obfuscate out
                else pure out
              pure $ (ews, out', out' /= originalContents)
        let
          customErrOrder LayoutWarning{} = -1 :: Int
          customErrOrder ErrorOutputCheck{} = 1
          customErrOrder ErrorUnknownNode{} = -2 :: Int
          customErrOrder ErrorMacroConfig{} = 4
        unless (null errsWarns) $ do
          let
            groupedErrsWarns =
              Data.List.Extra.groupOn customErrOrder
                $ List.sortOn customErrOrder
                $ errsWarns
          groupedErrsWarns `forM_` \case
            ErrorOutputCheck{} : _ -> do
              putStrErrLn
                $ "ERROR: brittany pretty printer"
                ++ " returned syntactically invalid result."
            uns@(ErrorUnknownNode{} : _) -> do
              putStrErrLn
                $ "WARNING: encountered unknown syntactical constructs:"
              uns `forM_` \case
                ErrorUnknownNode str ast@(L loc _) -> do
                  putStrErrLn $ "  " <> str <> " at " <> showSDocUnsafe (ppr loc)
                  when
                      (config
                      & _conf_debug
                      & _dconf_dump_ast_unknown
                      & confUnpack
                      )
                    $ do
                        putStrErrLn $ "  " ++ show (astToDoc ast)
                _ -> error "cannot happen (TM)"
              putStrErrLn
                "  -> falling back on exactprint for this element of the module"
            warns@(LayoutWarning{} : _) -> do
              putStrErrLn $ "WARNINGS:"
              warns `forM_` \case
                LayoutWarning str -> putStrErrLn str
                _ -> error "cannot happen (TM)"
            ErrorMacroConfig err input : _ -> do
              putStrErrLn $ "Error: parse error in inline configuration:"
              putStrErrLn err
              putStrErrLn $ "  in the string \"" ++ input ++ "\"."
            [] -> error "cannot happen"
        -- TODO: don't output anything when there are errors unless user
        -- adds some override?
        let
          hasErrors =
            if config & _conf_errorHandling & _econf_Werror & confUnpack
              then not $ null errsWarns
              else any ((> 0) . customErrOrder) errsWarns
          outputOnErrs =
            config
              & _conf_errorHandling
              & _econf_produceOutputOnErrors
              & confUnpack
          shouldOutput =
            not suppressOutput
              && not checkMode
              && (not hasErrors || outputOnErrs)

        when shouldOutput
          $ addTraceSep (_conf_debug config)
          $ case outputPathM of
              Nothing -> liftIO $ TIO.putStr $ outSText
              Just p -> liftIO $ do
                let
                  isIdentical = case inputPathM of
                    Nothing -> False
                    Just _ -> not hasChanges
                unless isIdentical $ TIO.writeFile p $ outSText

        when (checkMode && hasChanges) $ case inputPathM of
          Nothing -> pure ()
          Just p -> liftIO $ putStrLn $ "formatting would modify: " ++ p

        when hasErrors $ ExceptT.throwE 70
        return (if hasChanges then Changes else NoChanges)
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
