{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Haskell.Brittany.Internal.Types
  ( PPM
  , PPMLocal
  , BriDocF(..)
  , BriDoc
  , BriDocNumbered
  , ColsOrNewlines(..)
  , LayoutState(..)
  , lstate_baseY
  , lstate_indLevel
  , BriSpacing(..)
  , ColSig(..)
  , BrIndent(..)
  , ToBriDocM
  , DocMultiLine(..)
  , BrittanyWarning(..)
  , BrittanyError(..)
  , NodeAllocIndex(..)
  , unwrapBriDocNumbered
  , isNotEmpty
  , VerticalSpacingPar(..)
  , VerticalSpacing(..)
  , LineModeValidity(..)
  , pattern LineModeValid
  , pattern LineModeInvalid
  ) where

import Control.Comonad.Cofree
import Control.Monad.Trans.MultiRWS.Strict qualified as MultiRWSS
import Data.Data (Data)
import Data.Fix
import Data.Strict.Maybe qualified as Strict
import Data.Text.Lazy.Builder qualified as Text.Builder
import Data.Void
import GHC.Parser.Annotation
import GHC.Types.SrcLoc
import Prettyprinter (Doc)
import Safe qualified

import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils ()
import Language.Haskell.Brittany.Internal.RecursionSchemes

import GHC.PrettyInstances ()
import Prettyprinter.Generics

type PPM = MultiRWSS.MultiRWS
  '[Config]
  '[Text.Builder.Builder, [BrittanyError], Seq String]
  '[]

type PPMLocal = MultiRWSS.MultiRWS
  '[Config]
  '[Text.Builder.Builder, [BrittanyError], Seq String]
  '[]

data ColsOrNewlines
  -- Number of chars in the current line.
  = Cols {-# UNPACK #-} !Int
  -- Number of newlines to be inserted before inserting any non-space elements.
  | InsertNewlines {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

data LayoutState = LayoutState
  {

    -- Stack of number of current indentation columns (not number of
    -- indentations).
    _lstate_baseYs           :: [Int]

  , _lstate_curYOrAddNewline :: ColsOrNewlines


  -- Stack of current indentation levels. set for any layout-affected
  -- elements such as let/do/case/where elements. The main purpose of
  -- this member is to properly align comments, as their annotation
  -- positions are relative to the current layout indentation level.
  , _lstate_indLevels        :: [Int]

  -- Like a "last" of indLevel. Used for properly treating cases where
  -- comments on the first indented element have an annotation offset
  -- relative to the last non-indented element, which is confusing.
  , _lstate_indLevelLinger   :: !Int

  -- This communicates two things: firstly, that cursor is currently
  -- at the end of a comment (so needs newline before any actual
  -- content). secondly, the column at which insertion of comments
  -- started.
  , _lstate_commentCol       :: Maybe Int

  -- Number of spaces to insert if anyone writes (any non-spaces) in
  -- the current line.
  , _lstate_addSepSpace      :: Maybe Int

  -- Number of newlines inserted due to move-to-DP at a start of a
  -- comment. Necessary because some keyword DPs are relative to the
  -- last non-comment entity (for some reason). This is not very
  -- strictly reset to 0, so we might in some cases get "artifacts"
  -- from previous document elements. But the worst effect at the
  -- moment would be that we introduce less newlines on moveToKWDP,
  -- which seems harmless enough.
  , _lstate_commentNewlines  :: !Int
  }

lstate_baseY :: LayoutState -> Int
lstate_baseY = Safe.headNote "lstate_baseY" . _lstate_baseYs

lstate_indLevel :: LayoutState -> Int
lstate_indLevel = Safe.headNote "lstate_baseY" . _lstate_indLevels

-- evil, incomplete Show instance; only for debugging.
instance Show LayoutState where
  show state =
    "LayoutState"
    ++ "{baseYs=" ++ show (_lstate_baseYs state)
    ++ ",curYOrAddNewline=" ++ show (_lstate_curYOrAddNewline state)
    ++ ",indLevels=" ++ show (_lstate_indLevels state)
    ++ ",indLevelLinger=" ++ show (_lstate_indLevelLinger state)
    ++ ",commentCol=" ++ show (_lstate_commentCol state)
    ++ ",addSepSpace=" ++ show (_lstate_addSepSpace state)
    ++ ",commentNewlines=" ++ show (_lstate_commentNewlines state)
    ++ "}"

data BrittanyWarning
  = CPPWarning
  deriving (Eq)

data BrittanyError
  = ErrorMacroConfig String String
    -- ^ in-source config string parsing error; first argument is the parser
    --   output and second the corresponding, ill-formed input.
  | LayoutWarning String
    -- ^ some warning
  | ErrorUnknownNode String SrcSpan (Doc Void)
    -- ^ internal error: pretty-printing is not implemented for type of node
    --   in the syntax-tree
  | ErrorOutputCheck String
    -- ^ checking the output for syntactic validity failed


data BriSpacing = BriSpacing
  {
    -- Space in the current, potentially somewhat filled line.
    _bs_spacePastLineIndent :: !Int
    -- Space required in properly indented blocks below the current
    -- line.
  , _bs_spacePastIndent     :: !Int
  }

data ColSig
  = ColTyOpPrefix
    -- any prefixed operator/paren/"::"/..
    -- expected to have exactly two colums.
    -- e.g. ":: foo"
    --       111222
    --      "-> bar asd asd"
    --       11122222222222
  | ColPatternsFuncPrefix
    -- pattern-part of the lhs, e.g. "func (foo a b) c _".
    -- Has variable number of columns depending on the number of patterns.
  | ColPatternsFuncInfix
    -- pattern-part of the lhs, e.g. "Foo a <> Foo b".
    -- Has variable number of columns depending on the number of patterns.
  | ColPatterns
  | ColCasePattern
  | ColBindingLine (Maybe Text)
    -- e.g. "func pat pat = expr"
    --       1111111111111222222
    -- or   "pat | stmt -> expr"
    --       111111111112222222
    -- expected to have exactly two columns.
  | ColGuard
    -- e.g. "func pat pat | cond = ..."
    --       11111111111112222222
    -- or   "pat | cond1, cond2 -> ..."
    --       1111222222222222222
    -- expected to have exactly two columns
  | ColGuardedBody
    -- e.g. | foofoo = 1
    --      | bar    = 2
    --      111111111222
    -- expected to have exactly two columns
  | ColBindStmt
  | ColDoLet -- the non-indented variant
  | ColRec
  | ColRecUpdate -- used for both RecCon and RecUpd. TODO: refactor to reflect?
  | ColRecDecl
  | ColListComp
  | ColList
  | ColApp Text
  | ColTuple
  | ColTuples
  | ColOpPrefix -- merge with ColList ? other stuff?
  | ColImport
  deriving (Eq, Ord, Data, Show, Generic)
  deriving Pretty via PPGeneric ColSig

data BrIndent
  = BrIndentNone
  | BrIndentRegular
  | BrIndentSpecial !Int
  deriving (Eq, Ord, Data, Show, Generic)
  deriving Pretty via PPGeneric BrIndent

type ToBriDocM = MultiRWSS.MultiRWS
                   '[Config] -- reader
                   '[[BrittanyError], Seq String] -- writer
                   '[NodeAllocIndex] -- state

-- type ToBriDoc  an (sym :: Type -> Type) = LocatedAn an (sym GhcPs) -> ToBriDocM BriDocNumbered
-- type ToBriDoc' an sym                   = LocatedAn an sym         -> ToBriDocM BriDocNumbered
-- type ToBriDocC an sym c                 = LocatedAn an sym         -> ToBriDocM c

data DocMultiLine
  = MultiLineNo
  | MultiLinePossible
  deriving (Eq, Generic)
  deriving Pretty via PPGeneric DocMultiLine

data BriDocF a
  = BDEmpty
  | BDLit !Text
  | BDSeq [a]         -- elements other than the last should not contain BDPars.
  | BDCols ColSig [a] -- elements other than the last should not contain BDPars
  | BDSeparator                   -- semantically, space-unless-at-end-of-line.
  | BDAddBaseY BrIndent a
  | BDBaseYPushCur a
  | BDBaseYPop a
  | BDIndentLevelPushCur a
  | BDIndentLevelPop a
  | BDPar
    { _bdfpar_indent     :: BrIndent
    , _bdfpar_restOfLine :: a -- should not contain other BDPars
    , _bdfpar_indented   :: a
    }
  -- | BDAddIndent BrIndent (BriDocF f)
  -- | BDNewline
  | BDAlt [a]
  | BDForwardLineMode a
  | BDExternal
      Bool -- should print extra comment ?
      Text
  | BDPlain
      !Text -- used for QuasiQuotes, content can be multi-line (contrast to BDLit)
  | BDAnnotationBefore (EpAnn ()) a
  | BDAnnotationKW (Maybe AnnKeywordId) a
  | BDAnnotationAfter (EpAnn ()) a
  | BDMoveToKWDP
      AnnKeywordId
      Bool -- True if should respect x offset
      a
  | BDLines [a]
  | BDEnsureIndent BrIndent a
  | BDForceMultiline a
  | BDForceSingleline a
  | BDNonBottomSpacing Bool a
  | BDSetParSpacing a
  | BDForceParSpacing a
  | BDDebug String a
  deriving (Eq, Data, Functor, Foldable, Traversable, Generic)

deriving via PPGeneric (BriDocF a) instance Pretty a => Pretty (BriDocF a)

type BriDoc = Fix BriDocF
type BriDocNumbered = Cofree BriDocF Int

newtype NodeAllocIndex = NodeAllocIndex Int

-- TODO: rename to "dropLabels" ?
unwrapBriDocNumbered :: BriDocNumbered -> BriDoc
unwrapBriDocNumbered = cataAnn $ \_ -> Fix

isNotEmpty :: BriDoc -> Bool
isNotEmpty (Fix BDEmpty) = False
isNotEmpty _              = True

data VerticalSpacingPar
  -- No indented lines.
  = VerticalSpacingParNone
  -- Indented lines, requiring this much vertical space at most.
  | VerticalSpacingParSome   !Int
  -- Indented lines, requiring this much vertical space at most, but
  -- should be considered as having space for any spacing validity
  -- check.
  | VerticalSpacingParAlways !Int
    -- TODO: it might be wrong not to extend "always" to the none case, i.e.
    -- we might get better properties of spacing operators by having a
    -- product like (Normal|Always, None|Some Int).
  deriving (Eq, Show)

data VerticalSpacing = VerticalSpacing
  { _vs_sameLine  :: !Int
  , _vs_paragraph :: !VerticalSpacingPar
  , _vs_parFlag   :: !Bool
  } deriving (Eq, Show)

newtype LineModeValidity a = LineModeValidity (Strict.Maybe a)
  deriving (Functor, Applicative, Monad, Show, Alternative)

pattern LineModeValid :: forall t. t -> LineModeValidity t
pattern LineModeValid x = LineModeValidity (Strict.Just x) :: LineModeValidity t
pattern LineModeInvalid :: forall t. LineModeValidity t
pattern LineModeInvalid = LineModeValidity Strict.Nothing :: LineModeValidity t

{-# COMPLETE LineModeValid, LineModeInvalid #-}
