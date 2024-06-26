{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Language.Haskell.Brittany.Internal.Config.Types
  ( confUnpack
  , CLayoutConfig(..)
  , CForwardOptions(..)
  , CErrorHandlingConfig(..)
  , CPreProcessorConfig(..)
  , CConfig(..)

  , IndentPolicy(..)
  , AltChooser(..)
  , ColumnAlignMode(..)
  , CPPMode(..)
  , ExactPrintFallbackMode(..)

  , LayoutConfig
  , ForwardOptions
  , ErrorHandlingConfig
  , Config
  ) where

import Data.CZipWith
import Data.Coerce
import Data.Data (Data)
import Data.Functor.Identity
import Data.Semigroup (Last)
import Data.Semigroup.Generic
import GHC.Generics

confUnpack :: Coercible a b => Identity a -> b
confUnpack (Identity x) = coerce x

data CLayoutConfig f = LayoutConfig
  { _lconfig_cols :: f (Last Int) -- the thing that has default 80.
  , _lconfig_indentPolicy :: f (Last IndentPolicy)
  , _lconfig_indentAmount :: f (Last Int)
  , _lconfig_indentWhereSpecial :: f (Last Bool) -- indent where only 1 sometimes (TODO).
  , _lconfig_indentListSpecial :: f (Last Bool) -- use some special indentation for ","
                                                 -- when creating zero-indentation
                                                 -- multi-line list literals.
  , _lconfig_importColumn :: f (Last Int)
    -- ^ for import statement layouting, column at which to align the
    -- elements to be imported from a module.
    -- It is expected that importAsColumn >= importCol.
  , _lconfig_importAsColumn :: f (Last Int)
    -- ^ for import statement layouting, column at which put the module's
    -- "as" name (which also affects the positioning of the "as" keyword).
    -- It is expected that importAsColumn >= importCol.
  , _lconfig_altChooser :: f (Last AltChooser)
  , _lconfig_columnAlignMode :: f (Last ColumnAlignMode)
  , _lconfig_alignmentLimit :: f (Last Int)
    -- roughly speaking, this sets an upper bound to the number of spaces
    -- inserted to create horizontal alignment.
    -- More specifically, if 'xs' are the widths of the columns in some
    -- alignment-block, then the block will be aligned with the width
    -- maximum [ x | x <- xs, x < minimum xs + alignmentLimit ].
  , _lconfig_alignmentBreakOnMultiline :: f (Last Bool)
    -- stops alignment between items that are not layouted as a single line.
    -- e.g. for single-line alignment, things remain unchanged:
    --   do
    --     short       <- stuff
    --     loooooooong <- stuff
    -- but not in cases such as:
    --   do
    --     short <- some more stuff
    --       that requires two lines
    --     loooooooong <- stuff
  , _lconfig_hangingTypeSignature :: f (Last Bool)
    -- Do not put "::" in a new line, and use hanging indentation for the
    -- signature, i.e.:
    -- func :: SomeLongStuff
    --      -> SomeLongStuff
    -- instead of the usual
    -- func
    --   :: SomeLongStuff
    --   -> SomeLongStuff
    -- As usual for hanging indentation, the result will be
    -- context-sensitive (in the function name).
  , _lconfig_reformatModulePreamble :: f (Last Bool)
    -- whether the module preamble/header (module keyword, name, export list,
    -- import statements) are reformatted. If false, only the elements of the
    -- module (everything past the "where") are reformatted.
  , _lconfig_allowSingleLineExportList :: f (Last Bool)
    -- if true, and it fits in a single line, and there are no comments in the
    -- export list, the following layout will be used:
    -- > module MyModule (abc, def) where
    -- > [stuff]
    -- otherwise, the multi-line version is used:
    -- > module MyModule
    -- >   ( abc
    -- >   , def
    -- >   )
    -- > where
  , _lconfig_allowHangingQuasiQuotes :: f (Last Bool)
    -- if false, the layouter sees any splices as infinitely big and places
    -- them accordingly (in newlines, most likely); This also influences
    -- parent nodes.
    -- if true, the layouter is free to start a quasi-quotation at the end
    -- of a line.
    --
    -- false:
    -- > let
    -- >   body =
    -- >     [json|
    -- >     hello
    -- >     |]
    --
    -- true:
    -- > let body = [json|
    -- >     hello
    -- >     |]
  -- , _lconfig_allowSinglelineRecord :: f (Last Bool)
  --   -- if true, layouts record data decls as a single line when possible, e.g.
  --   -- > MyPoint { x :: Double, y :: Double }
  --   -- if false, always use the multi-line layout
  --   -- > MyPoint
  --   -- >   { x :: Double
  --   -- >   , y :: Double
  --   -- >   }
  } deriving Generic

data CForwardOptions f = ForwardOptions
  { _options_ghc :: f [String]
  } deriving Generic

data CErrorHandlingConfig f = ErrorHandlingConfig
  { _econf_produceOutputOnErrors :: f (Last Bool)
  , _econf_Werror :: f (Last Bool)
  , _econf_ExactPrintFallback :: f (Last ExactPrintFallbackMode)
    -- ^ Determines when to fall back on the exactprint'ed output when
    -- syntactical constructs are encountered which are not yet handled by
    -- brittany.
    -- Note that the "risky" setting is risky because even with the check of
    -- the syntactic validity of the brittany output, at least in theory there
    -- may be cases where the output is syntactically/semantically valid but
    -- has different semantics than the code pre-transformation.
  , _econf_omit_output_valid_check :: f (Last Bool)
  }
  deriving Generic

data CPreProcessorConfig f = PreProcessorConfig
  { _ppconf_CPPMode :: f (Last CPPMode)
  , _ppconf_hackAroundIncludes :: f (Last Bool)
    -- ^ The flag will do the following: insert a marker string
    -- ("-- BRITANY_INCLUDE_HACK ") right before any lines starting with
    -- "#include" before processing (parsing) input; and remove that marker
    -- string from the transformation output.
    -- The flag is intentionally misspelled to prevent clashing with
    -- inline-config stuff.
  } deriving Generic

data CConfig f = Config
  { _conf_version                   :: f (Last Int)
  , _conf_layout                    :: CLayoutConfig f
  , _conf_errorHandling             :: CErrorHandlingConfig f
  , _conf_forward                   :: CForwardOptions f
  , _conf_preprocessor              :: CPreProcessorConfig f
    -- This field is somewhat of a duplicate of the one in
    -- DebugConfig. It is used for per-declaration disabling by the
    -- inline config implementation. Could have re-used the existing
    -- field, but felt risky to use a "debug" labeled field for
    -- non-debug functionality.
  , _conf_roundtrip_exactprint_only :: f (Last Bool)
    -- Used for inline config that disables brittany entirely for this
    -- module. Useful for wildcard application (`find -name "*.hs" |
    -- xargs brittany --write-mode inplace` or something in that
    -- direction).
  , _conf_disable_formatting        :: f (Last Bool)
  , _conf_obfuscate                 :: f (Last Bool)
  } deriving Generic

type LayoutConfig        = CLayoutConfig Identity
type ForwardOptions      = CForwardOptions Identity
type ErrorHandlingConfig = CErrorHandlingConfig Identity
type Config              = CConfig Identity

deriving instance Show (CLayoutConfig Identity)
deriving instance Show (CErrorHandlingConfig Identity)
deriving instance Show (CForwardOptions Identity)
deriving instance Show (CPreProcessorConfig Identity)
deriving instance Show (CConfig Identity)

deriving instance Show (CLayoutConfig Maybe)
deriving instance Show (CErrorHandlingConfig Maybe)
deriving instance Show (CForwardOptions Maybe)
deriving instance Show (CPreProcessorConfig Maybe)
deriving instance Show (CConfig Maybe)

deriving instance Data (CLayoutConfig Identity)
deriving instance Data (CErrorHandlingConfig Identity)
deriving instance Data (CForwardOptions Identity)
deriving instance Data (CPreProcessorConfig Identity)
deriving instance Data (CConfig Identity)

deriving instance Data (CLayoutConfig Maybe)
deriving instance Data (CErrorHandlingConfig Maybe)
deriving instance Data (CForwardOptions Maybe)
deriving instance Data (CPreProcessorConfig Maybe)
deriving instance Data (CConfig Maybe)

instance Semigroup (CLayoutConfig Maybe) where
  (<>) = gmappend
instance Semigroup (CErrorHandlingConfig Maybe) where
  (<>) = gmappend
instance Semigroup (CForwardOptions Maybe) where
  (<>) = gmappend
instance Semigroup (CPreProcessorConfig Maybe) where
  (<>) = gmappend
instance Semigroup (CConfig Maybe) where
  (<>) = gmappend

instance Semigroup (CLayoutConfig Identity) where
  (<>) = gmappend
instance Semigroup (CErrorHandlingConfig Identity) where
  (<>) = gmappend
instance Semigroup (CForwardOptions Identity) where
  (<>) = gmappend
instance Semigroup (CPreProcessorConfig Identity) where
  (<>) = gmappend
instance Semigroup (CConfig Identity) where
  (<>) = gmappend

instance Monoid (CLayoutConfig Maybe) where
  mempty = gmempty
instance Monoid (CErrorHandlingConfig Maybe) where
  mempty = gmempty
instance Monoid (CForwardOptions Maybe) where
  mempty = gmempty
instance Monoid (CPreProcessorConfig Maybe) where
  mempty = gmempty
instance Monoid (CConfig Maybe) where
  mempty = gmempty

data IndentPolicy
  -- Never create a new indentation at more than old indentation + amount.
  = IndentPolicyLeft
  -- Can create new indentations whereever.
  | IndentPolicyFree
  -- Can create indentations only at any n * amount.
  | IndentPolicyMultiple
  deriving (Eq, Show, Generic, Data)

data AltChooser
  -- Always choose last alternative. leads to tons of sparsely filled
  -- lines.
  = AltChooserSimpleQuick
  -- Choose the first matching alternative using the simplest spacing
  -- information for the children.
  | AltChooserShallowBest
  -- Choose the first matching alternative using a bounded list of
  -- recursive options having sufficient space.
  | AltChooserBoundedSearch !Int
  deriving (Show, Generic, Data)

data ColumnAlignMode
  = ColumnAlignModeDisabled
    -- ^ Make no column alignments whatsoever
  | ColumnAlignModeUnanimously
    -- ^ Make column alignments only if it does not cause overflow for any of
    -- the affected lines.
  | ColumnAlignModeMajority Float
    -- ^ If at least (ratio::Float) of the aligned elements have sufficient
    -- space for the alignment, act like ColumnAlignModeAnimously; otherwise
    -- act like ColumnAlignModeDisabled.
  | ColumnAlignModeAnimouslyScale Int
    -- ^ Scale back columns to some degree if their sum leads to overflow.
    -- This is done in a linear fashion.
    -- The Int specifies additional columns to be added to column maximum for
    -- scaling calculation purposes.
  | ColumnAlignModeAnimously
    -- ^ Decide on a case-by-case basis if alignment would cause overflow.
    -- If it does, cancel all alignments for this (nested) column description.
    -- ColumnAlignModeAnimouslySome -- potentially to implement
  | ColumnAlignModeAlways
    -- ^ Always respect column alignments, even if it makes stuff overflow.
  deriving (Show, Generic, Data)

data CPPMode
  = CPPModeAbort  -- Abort program on seeing -XCPP
  | CPPModeWarn   -- Warn about CPP and non-roundtripping in its presence.
  | CPPModeNowarn -- Silently allow CPP, if possible (i.e. input is file.)
  deriving (Show, Generic, Data)

data ExactPrintFallbackMode
  = ExactPrintFallbackModeNever  -- Never fall back on exactprinting
  | ExactPrintFallbackModeInline -- Fall back only if there are no newlines in
                                 -- the exactprint'ed output.
  | ExactPrintFallbackModeRisky  -- Fall back even in the presence of newlines.
                                 -- THIS MAY THEORETICALLY CHANGE SEMANTICS OF
                                 -- A PROGRAM BY TRANSFORMING IT.
  deriving (Show, Generic, Data)

deriveCZipWith ''CLayoutConfig
deriveCZipWith ''CErrorHandlingConfig
deriveCZipWith ''CForwardOptions
deriveCZipWith ''CPreProcessorConfig
deriveCZipWith ''CConfig

instance CFunctor CLayoutConfig
instance CFunctor CErrorHandlingConfig
instance CFunctor CForwardOptions
instance CFunctor CPreProcessorConfig
instance CFunctor CConfig
