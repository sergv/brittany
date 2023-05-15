{-# OPTIONS_GHC -Wno-implicit-prelude #-}

module Language.Haskell.Brittany.Internal.ParseModule
  ( parseModuleFromString
  ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Except as Except
import qualified GHC
import qualified GHC.ByteOrder
import qualified GHC.Data.StringBuffer
import qualified GHC.Driver.Config.Parser
import qualified GHC.Driver.Errors.Types
import qualified GHC.Driver.Session
import qualified GHC.Parser.Header
import qualified GHC.Parser.Lexer
import qualified GHC.Platform
import qualified GHC.Settings
import qualified GHC.Types.SafeHaskell as GHC.Driver.Session
import qualified GHC.Types.SrcLoc
import qualified GHC.Utils.Fingerprint
import qualified GHC.Utils.Outputable
import Language.Haskell.GHC.ExactPrint (makeDeltaAst)
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint

-- | Parses a Haskell module. Although this nominally requires IO, it is
-- morally pure. It should have no observable effects.
parseModuleFromString
  :: MonadIO m
  => [String]
  -> FilePath
  -> (GHC.Driver.Session.DynFlags -> m (Either String a))
  -> String
  -> m (Either String (GHC.ParsedSource, a))
parseModuleFromString arguments1 filePath checkDynFlags string = Except.runExceptT $ do
  let
    dynFlags1 = GHC.Driver.Session.gopt_set
      -- It feels like this should be either @Sf_Ignore@ or @Sf_None@, but both
      -- of those modes have trouble parsing safe imports (@import safe ...@).
      -- Neither passing in @"-XUnsafe"@ as a command line argument nor having
      -- @{-# LANGUAGE Unsafe #-}@ in the source file seem to help.
      initialDynFlags
        { GHC.Driver.Session.safeHaskell = GHC.Driver.Session.Sf_Unsafe
        }
      GHC.Driver.Session.Opt_KeepRawTokenStream
  (dynFlags2, leftovers1, _) <- GHC.Driver.Session.parseDynamicFlagsCmdLine
    dynFlags1
    (GHC.Types.SrcLoc.noLoc <$> arguments1)
  handleLeftovers leftovers1
  let stringBuffer = GHC.Data.StringBuffer.stringToStringBuffer string
      parserOpts :: GHC.Parser.Lexer.ParserOpts
      parserOpts = GHC.Driver.Config.Parser.initParserOpts dynFlags2
      (_warns, arguments2)   = GHC.Parser.Header.getOptions parserOpts stringBuffer filePath
  (dynFlags3, leftovers2, _) <- GHC.Driver.Session.parseDynamicFilePragma
    dynFlags2
    arguments2
  handleLeftovers leftovers2
  dynFlagsResult <- Except.ExceptT $ checkDynFlags dynFlags3
  let parseResult :: ExactPrint.ParseResult GHC.ParsedSource
      parseResult = ExactPrint.parseModuleFromStringInternal dynFlags3 filePath string
  case parseResult of
    Left errorMessages -> handleErrorMessages errorMessages
    Right parsedSource -> pure (makeDeltaAst parsedSource, dynFlagsResult)

handleLeftovers
  :: Monad m => [GHC.Types.SrcLoc.Located String] -> Except.ExceptT String m ()
handleLeftovers leftovers
  = unless (null leftovers)
  $ Except.throwE
  $ "leftovers: " <> show (GHC.Types.SrcLoc.unLoc <$> leftovers)

handleErrorMessages
  :: Monad m => GHC.Driver.Errors.Types.ErrorMessages -> Except.ExceptT String m a
handleErrorMessages
  = Except.throwE
  . mappend "errorMessages: "
  . GHC.Utils.Outputable.showPprUnsafe

initialDynFlags :: GHC.Driver.Session.DynFlags
initialDynFlags = GHC.Driver.Session.defaultDynFlags initialSettings

initialSettings :: GHC.Driver.Session.Settings
initialSettings = GHC.Driver.Session.Settings
  { GHC.Driver.Session.sGhcNameVersion = initialGhcNameVersion
  , GHC.Driver.Session.sFileSettings   = initialFileSettings
  , GHC.Driver.Session.sTargetPlatform = initialTargetPlatform
  , GHC.Driver.Session.sToolSettings   = initialToolSettings
  , GHC.Driver.Session.sPlatformMisc   = initialPlatformMisc
  , GHC.Driver.Session.sRawSettings    = []
  }

initialFileSettings :: GHC.Driver.Session.FileSettings
initialFileSettings = GHC.Driver.Session.FileSettings
  { GHC.Driver.Session.fileSettings_ghciUsagePath         = ""
  , GHC.Driver.Session.fileSettings_ghcUsagePath          = ""
  , GHC.Driver.Session.fileSettings_globalPackageDatabase = ""
  , GHC.Driver.Session.fileSettings_toolDir               = Nothing
  , GHC.Driver.Session.fileSettings_topDir                = ""
  }

initialGhcNameVersion :: GHC.Driver.Session.GhcNameVersion
initialGhcNameVersion = GHC.Driver.Session.GhcNameVersion
  { GHC.Driver.Session.ghcNameVersion_programName = ""
  , GHC.Driver.Session.ghcNameVersion_projectVersion = ""
  }

initialPlatformMisc :: GHC.Driver.Session.PlatformMisc
initialPlatformMisc = GHC.Driver.Session.PlatformMisc
  { GHC.Driver.Session.platformMisc_ghcWithInterpreter = False
  , GHC.Driver.Session.platformMisc_libFFI = False
  , GHC.Driver.Session.platformMisc_llvmTarget = ""
  , GHC.Driver.Session.platformMisc_targetPlatformString = ""
  }

initialTargetPlatform :: GHC.Settings.Platform
initialTargetPlatform = GHC.Settings.Platform
  { GHC.Settings.platformByteOrder                = GHC.ByteOrder.LittleEndian
  , GHC.Settings.platformHasGnuNonexecStack       = False
  , GHC.Settings.platformHasIdentDirective        = False
  , GHC.Settings.platformHasSubsectionsViaSymbols = False
  , GHC.Settings.platformIsCrossCompiling         = False
  , GHC.Settings.platformLeadingUnderscore        = False
  , GHC.Settings.platformTablesNextToCode         = False
  , GHC.Settings.platformUnregisterised           = False
  , GHC.Settings.platformWordSize                 = GHC.Platform.PW8
  , GHC.Settings.platformArchOS                   = GHC.Platform.ArchOS GHC.Platform.ArchUnknown GHC.Platform.OSUnknown
  , GHC.Settings.platform_constants               = Nothing
  , GHC.Settings.platformHasLibm                  = False
  }

initialToolSettings :: GHC.Settings.ToolSettings
initialToolSettings = GHC.Settings.ToolSettings
  { GHC.Settings.toolSettings_ccSupportsNoPie         = False
  , GHC.Settings.toolSettings_extraGccViaCFlags       = []
  , GHC.Settings.toolSettings_ldIsGnuLd               = False
  -- , GHC.Settings.toolSettings_ldSupportsBuildId       = False
  , GHC.Settings.toolSettings_ldSupportsCompactUnwind = False
  , GHC.Settings.toolSettings_ldSupportsFilelist      = False
  , GHC.Settings.toolSettings_opt_a                   = []
  , GHC.Settings.toolSettings_opt_c                   = []
  , GHC.Settings.toolSettings_opt_cxx                 = []
  , GHC.Settings.toolSettings_opt_F                   = []
  , GHC.Settings.toolSettings_opt_i                   = []
  , GHC.Settings.toolSettings_opt_l                   = []
  , GHC.Settings.toolSettings_opt_L                   = []
  , GHC.Settings.toolSettings_opt_lc                  = []
  , GHC.Settings.toolSettings_opt_lcc                 = []
  , GHC.Settings.toolSettings_opt_lm                  = []
  , GHC.Settings.toolSettings_opt_lo                  = []
  , GHC.Settings.toolSettings_opt_P                   = []
  , GHC.Settings.toolSettings_opt_P_fingerprint       =
    GHC.Utils.Fingerprint.fingerprint0
  , GHC.Settings.toolSettings_opt_windres             = []
  , GHC.Settings.toolSettings_pgm_a                   = ("", [])
  , GHC.Settings.toolSettings_pgm_ar                  = ""
  , GHC.Settings.toolSettings_pgm_c                   = ""
  , GHC.Settings.toolSettings_pgm_dll                 = ("", [])
  , GHC.Settings.toolSettings_pgm_F                   = ""
  , GHC.Settings.toolSettings_pgm_i                   = ""
  , GHC.Settings.toolSettings_pgm_install_name_tool   = ""
  , GHC.Settings.toolSettings_pgm_l                   = ("", [])
  , GHC.Settings.toolSettings_pgm_L                   = ""
  , GHC.Settings.toolSettings_pgm_lc                  = ("", [])
  , GHC.Settings.toolSettings_pgm_lcc                 = ("", [])
  -- , GHC.Settings.toolSettings_pgm_libtool          = ""
  , GHC.Settings.toolSettings_pgm_lm                  = Nothing
  , GHC.Settings.toolSettings_pgm_lo                  = ("", [])
  , GHC.Settings.toolSettings_pgm_otool               = ""
  , GHC.Settings.toolSettings_pgm_P                   = ("", [])
  , GHC.Settings.toolSettings_pgm_ranlib              = ""
  , GHC.Settings.toolSettings_pgm_T                   = ""
  , GHC.Settings.toolSettings_pgm_windres             = ""
  , GHC.Settings.toolSettings_useInplaceMinGW         = True
  , GHC.Settings.toolSettings_arSupportsDashL         = True
  , GHC.Settings.toolSettings_pgm_cxx                 = ""
  }