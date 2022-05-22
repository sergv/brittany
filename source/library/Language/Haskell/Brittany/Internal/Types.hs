{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Language.Haskell.Brittany.Internal.Types where

import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import Data.Data (Data)
import Data.Generics.Uniplate.Direct as Uniplate
import Data.Kind (Type)
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text.Lazy.Builder as Text.Builder
import GHC (AnnKeywordId, GenLocated, LocatedAn, SrcSpan)
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Prelude
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Safe
import GHC.Parser.Annotation

type PPM = MultiRWSS.MultiRWS
  '[Config]
  '[Text.Builder.Builder, [BrittanyError], Seq String]
  '[]

type PPMLocal = MultiRWSS.MultiRWS
  '[Config]
  '[Text.Builder.Builder, [BrittanyError], Seq String]
  '[]

-- newtype TopLevelDeclNameMap = TopLevelDeclNameMap (Map ExactPrint.AnnKey String)

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
  | forall ast ann. Data ast => ErrorUnknownNode String (LocatedAn ann ast)
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
  deriving (Eq, Ord, Data, Show)

data BrIndent
  = BrIndentNone
  | BrIndentRegular
  | BrIndentSpecial !Int
  deriving (Eq, Ord, Data, Show)

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
  deriving (Eq)

-- isomorphic to BriDocF Identity. Provided for ease of use, as we do a lot
-- of transformations on `BriDocF Identity`s and it is really annoying to
-- `Identity`/`runIdentity` everywhere.
data BriDoc
  = BDEmpty
  | BDLit !Text
  | BDSeq [BriDoc] -- elements other than the last should
                   -- not contains BDPars.
  | BDCols ColSig [BriDoc] -- elements other than the last
                         -- should not contains BDPars
  | BDSeparator -- semantically, space-unless-at-end-of-line.
  | BDAddBaseY BrIndent BriDoc
  | BDBaseYPushCur BriDoc
  | BDBaseYPop BriDoc
  | BDIndentLevelPushCur BriDoc
  | BDIndentLevelPop BriDoc
  | BDPar
    { _bdpar_indent     :: BrIndent
    , _bdpar_restOfLine :: BriDoc -- should not contain other BDPars
    , _bdpar_indented   :: BriDoc
    }
  -- | BDAddIndent BrIndent (BriDocF f)
  -- | BDNewline
  | BDAlt [BriDoc]
  | BDForwardLineMode BriDoc
  | BDExternal
      Bool -- should print extra comment ?
      Text
  | BDPlain
      !Text -- used for QuasiQuotes, content can be multi-line
            -- (contrast to BDLit)
  | BDAnnotationPrior BriDoc
  | BDAnnotationKW (Maybe AnnKeywordId) BriDoc
  | BDAnnotationRest  BriDoc
  | BDMoveToKWDP
      AnnKeywordId
      Bool -- True if should respect x offset
      BriDoc
  | BDLines [BriDoc]
  | BDEnsureIndent BrIndent BriDoc
  -- the following constructors are only relevant for the alt transformation
  -- and are removed afterwards. They should never occur in any BriDoc
  -- after the alt transformation.
  | BDForceMultiline BriDoc
  | BDForceSingleline BriDoc
  | BDNonBottomSpacing Bool BriDoc
  | BDSetParSpacing BriDoc
  | BDForceParSpacing BriDoc
  -- pseudo-deprecated
  | BDDebug String BriDoc
  deriving (Data, Eq, Ord)

data BriDocF f
  = BDFEmpty
  | BDFLit !Text
  | BDFSeq [f (BriDocF f)] -- elements other than the last should
                   -- not contains BDPars.
  | BDFCols ColSig [f (BriDocF f)] -- elements other than the last
                         -- should not contains BDPars
  | BDFSeparator -- semantically, space-unless-at-end-of-line.
  | BDFAddBaseY BrIndent (f (BriDocF f))
  | BDFBaseYPushCur (f (BriDocF f))
  | BDFBaseYPop (f (BriDocF f))
  | BDFIndentLevelPushCur (f (BriDocF f))
  | BDFIndentLevelPop (f (BriDocF f))
  | BDFPar
    { _bdfpar_indent     :: BrIndent
    , _bdfpar_restOfLine :: f (BriDocF f) -- should not contain other BDPars
    , _bdfpar_indented   :: f (BriDocF f)
    }
  -- | BDAddIndent BrIndent (BriDocF f)
  -- | BDNewline
  | BDFAlt [f (BriDocF f)]
  | BDFForwardLineMode (f (BriDocF f))
  | BDFExternal
      Bool -- should print extra comment ?
      Text
  | BDFPlain
      !Text -- used for QuasiQuotes, content can be multi-line
            -- (contrast to BDLit)
  | BDFAnnotationPrior (f (BriDocF f))
  | BDFAnnotationKW (Maybe AnnKeywordId) (f (BriDocF f))
  | BDFAnnotationRest  (f (BriDocF f))
  | BDFMoveToKWDP
      AnnKeywordId
      Bool -- True if should respect x offset
      (f (BriDocF f))
  | BDFLines [(f (BriDocF f))]
  | BDFEnsureIndent BrIndent (f (BriDocF f))
  | BDFForceMultiline (f (BriDocF f))
  | BDFForceSingleline (f (BriDocF f))
  | BDFNonBottomSpacing Bool (f (BriDocF f))
  | BDFSetParSpacing (f (BriDocF f))
  | BDFForceParSpacing (f (BriDocF f))
  | BDFDebug String (f (BriDocF f))

-- deriving instance Data (BriDocF Identity)
deriving instance Data (BriDocF ((,) Int))

type BriDocFInt = BriDocF ((,) Int)
type BriDocNumbered = (Int, BriDocFInt)

instance Uniplate.Uniplate BriDoc where
  uniplate x@BDEmpty{}               = plate x
  uniplate x@BDLit{}                 = plate x
  uniplate (BDSeq list     )         = plate BDSeq ||* list
  uniplate (BDCols sig list)         = plate BDCols |- sig ||* list
  uniplate x@BDSeparator             = plate x
  uniplate (BDAddBaseY ind bd      ) = plate BDAddBaseY |- ind |* bd
  uniplate (BDBaseYPushCur       bd) = plate BDBaseYPushCur |* bd
  uniplate (BDBaseYPop           bd) = plate BDBaseYPop |* bd
  uniplate (BDIndentLevelPushCur bd) = plate BDIndentLevelPushCur |* bd
  uniplate (BDIndentLevelPop     bd) = plate BDIndentLevelPop |* bd
  uniplate (BDPar ind line indented) = plate BDPar |- ind |* line |* indented
  uniplate (BDAlt             alts ) = plate BDAlt ||* alts
  uniplate (BDForwardLineMode bd   ) = plate BDForwardLineMode |* bd
  uniplate x@BDExternal{}            = plate x
  uniplate x@BDPlain{}               = plate x
  uniplate (BDAnnotationPrior bd)    = plate BDAnnotationPrior |* bd
  uniplate (BDAnnotationKW kw bd)    = plate BDAnnotationKW |- kw |* bd
  uniplate (BDAnnotationRest bd)     = plate BDAnnotationRest |* bd
  uniplate (BDMoveToKWDP kw b bd)    = plate BDMoveToKWDP |- kw |- b |* bd
  uniplate (BDLines lines          ) = plate BDLines ||* lines
  uniplate (BDEnsureIndent ind bd  ) = plate BDEnsureIndent |- ind |* bd
  uniplate (BDForceMultiline  bd   ) = plate BDForceMultiline |* bd
  uniplate (BDForceSingleline bd   ) = plate BDForceSingleline |* bd
  uniplate (BDNonBottomSpacing b bd) = plate BDNonBottomSpacing |- b |* bd
  uniplate (BDSetParSpacing   bd   ) = plate BDSetParSpacing |* bd
  uniplate (BDForceParSpacing bd   ) = plate BDForceParSpacing |* bd
  uniplate (BDDebug s bd           ) = plate BDDebug |- s |* bd

newtype NodeAllocIndex = NodeAllocIndex Int

-- TODO: rename to "dropLabels" ?
unwrapBriDocNumbered :: BriDocNumbered -> BriDoc
unwrapBriDocNumbered tpl = case snd tpl of
  BDFEmpty                 -> BDEmpty
  BDFLit t                 -> BDLit t
  BDFSeq list              -> BDSeq $ rec <$> list
  BDFCols sig list         -> BDCols sig $ rec <$> list
  BDFSeparator             -> BDSeparator
  BDFAddBaseY ind bd       -> BDAddBaseY ind $ rec bd
  BDFBaseYPushCur       bd -> BDBaseYPushCur $ rec bd
  BDFBaseYPop           bd -> BDBaseYPop $ rec bd
  BDFIndentLevelPushCur bd -> BDIndentLevelPushCur $ rec bd
  BDFIndentLevelPop     bd -> BDIndentLevelPop $ rec bd
  BDFPar ind line indented -> BDPar ind (rec line) (rec indented)
  BDFAlt             alts  -> BDAlt $ rec <$> alts -- not that this will happen
  BDFForwardLineMode bd    -> BDForwardLineMode $ rec bd
  BDFExternal c t          -> BDExternal c t
  BDFPlain t               -> BDPlain t
  BDFAnnotationPrior bd    -> BDAnnotationPrior $ rec bd
  BDFAnnotationKW kw bd    -> BDAnnotationKW kw $ rec bd
  BDFAnnotationRest bd     -> BDAnnotationRest $ rec bd
  BDFMoveToKWDP kw b bd    -> BDMoveToKWDP kw b $ rec bd
  BDFLines lines           -> BDLines $ rec <$> lines
  BDFEnsureIndent ind bd   -> BDEnsureIndent ind $ rec bd
  BDFForceMultiline  bd    -> BDForceMultiline $ rec bd
  BDFForceSingleline bd    -> BDForceSingleline $ rec bd
  BDFNonBottomSpacing b bd -> BDNonBottomSpacing b $ rec bd
  BDFSetParSpacing   bd    -> BDSetParSpacing $ rec bd
  BDFForceParSpacing bd    -> BDForceParSpacing $ rec bd
  BDFDebug s bd            -> BDDebug (s ++ "@" ++ show (fst tpl)) $ rec bd
  where
    rec = unwrapBriDocNumbered

isNotEmpty :: BriDoc -> Bool
isNotEmpty BDEmpty = False
isNotEmpty _       = True

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

data VerticalSpacing
  = VerticalSpacing
    { _vs_sameLine  :: !Int
    , _vs_paragraph :: !VerticalSpacingPar
    , _vs_parFlag   :: !Bool
    }
  deriving (Eq, Show)

newtype LineModeValidity a = LineModeValidity (Strict.Maybe a)
  deriving (Functor, Applicative, Monad, Show, Alternative)

pattern LineModeValid :: forall t. t -> LineModeValidity t
pattern LineModeValid x = LineModeValidity (Strict.Just x) :: LineModeValidity t
pattern LineModeInvalid :: forall t. LineModeValidity t
pattern LineModeInvalid = LineModeValidity Strict.Nothing :: LineModeValidity t
