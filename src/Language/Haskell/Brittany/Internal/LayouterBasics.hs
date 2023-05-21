{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Haskell.Brittany.Internal.LayouterBasics
  ( processDefault
  , briDocByExact
  , briDocByExactNoComment
  , briDocByExactNoComment'
  , briDocByExactInlineOnly
  , rdrNameToText
  , lrdrNameToText
  , nameAdornment
  , lrdrNameToTextAnnGen
  , lrdrNameToTextAnn
  , isDataTypeEquality
  , specialCaseDataTypeEquality
  , lrdrNameToTextAnnTypeEqualityIsSpecial
  , extractAllComments
  , extractFollowingComments
  , hasAnyCommentsBelow
  , hasCommentsBetween
  , hasAnyCommentsConnected
  , hasAnyRegularCommentsConnected
  , isRegularComment
  , astFoldConnectedComments
  , hasAnyRegularCommentsRest
  , hasAnnKeywordComment
  , hasAnnKeyword
  , allocateNode
  , allocNodeIndex
  , docEmpty
  , docLit
  , docLitS
  , docExt
  , docAlt
  , CollectAltM
  , addAlternativeCond
  , addAlternative
  , runFilteredAlternative
  , docSeq
  , docLines
  , docCols
  , docAddBaseY
  , docSetBaseY
  , docSetIndentLevel
  , docSetBaseAndIndent
  , docSeparator
  , docAnnotationKW
  , docMoveToKWDP
  , docNonBottomSpacing
  , docNonBottomSpacingS
  , docSetParSpacing
  , docForceParSpacing
  , docDebug
  , appSep
  , docCommaSep
  , docParenLSep
  , docParenL
  , docParenR
  , docParenHashLSep
  , docParenHashRSep
  , docBracketL
  , docBracketR
  , docTick
  , docNodeAnnKW
  , docNodeMoveToKWDP
  , DocWrapable(..)
  , forgetAnn
  , wrapBefore
  , wrapAfter
  , docPar
  , docForceSingleline
  , docForceMultiline
  , docEnsureIndent
  , unknownNodeError
  , spacifyDocs
  , briDocMToPPM
  , briDocMToPPMInner
  , docSharedWrapper
  ) where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans.MultiRWS (MonadMultiReader(..), MonadMultiState(..), MonadMultiWriter(..), mGet)
import Control.Monad.Trans.MultiRWS.Strict qualified as MultiRWSS
import Control.Monad.Writer.Strict
import Data.Char
import Data.Data (Data)
import Data.Functor.Identity
import Data.Generics qualified as SYB
import Data.List qualified as L
import Data.Maybe
import Data.Monoid as Monoid
import Data.Occurrences
import Data.Semigroup
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as TLB
import Data.Typeable
import Prettyprinter (Pretty(..))

import GHC (GenLocated(L), moduleName, moduleNameString)
import GHC.Parser.Annotation as GHC
import GHC.PrettyInstances ()
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName(..))
import GHC.Types.SrcLoc (getLoc)
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.GHC.ExactPrint (ExactPrint(..), exactPrint)
import Language.Haskell.GHC.ExactPrint qualified as ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Types qualified as ExactPrint
import Language.Haskell.GHC.ExactPrint.Types qualified as ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils (ss2posEnd, ss2pos)
import Language.Haskell.GHC.ExactPrint.Utils qualified as ExactPrint.Utils

processDefault
  :: ( MonadMultiWriter TLB.Builder m
     , ExactPrint a
     )
  => a
  -> m ()
processDefault x = do
  -- this hack is here so our print-empty-module trick does not add
  -- a newline at the start if there actually is no module header / imports
  -- / anything.
  -- TODO: instead the appropriate annotation could be removed when "cleaning"
  --       the module (header). This would remove the need for this hack!
  case exactPrint x of
    "\n" -> pure ()
    str  -> mTell $ TLB.fromString str

-- | Use ExactPrint's output for this node; add a newly generated inline comment
-- at insertion position (meant to point out to the user that this node is
-- not handled by brittany yet). Useful when starting implementing new
-- syntactic constructs when children are not handled yet.
briDocByExact
  :: ExactPrint (LocatedAn ann ast)
  => LocatedAn ann ast
  -> ToBriDocM BriDocNumbered
briDocByExact x = docExt id x True

-- | Use ExactPrint's output for this node.
-- Consider that for multi-line input, the indentation of the code produced
-- by ExactPrint might be different, and even incompatible with the indentation
-- of its surroundings as layouted by brittany. But there are safe uses of
-- this, e.g. for any top-level declarations.
briDocByExactNoComment
  :: ExactPrint (LocatedAn ann ast)
  => LocatedAn ann ast
  -> ToBriDocM BriDocNumbered
briDocByExactNoComment = briDocByExactNoComment' id

briDocByExactNoComment'
  :: ExactPrint (LocatedAn ann ast)
  => (Text -> Text)
  -> LocatedAn ann ast
  -> ToBriDocM BriDocNumbered
briDocByExactNoComment' f x = docExt f x False

-- | Use ExactPrint's output for this node, presuming that this output does
-- not contain any newlines. If this property is not met, the semantics
-- depend on the @econf_AllowRiskyExactPrintUse@ config flag.
briDocByExactInlineOnly
  :: (Typeable ann, Data ast, Pretty ann, Pretty ast)
  => ExactPrint (LocatedAn ann ast)
  => String
  -> LocatedAn ann ast
  -> ToBriDocM BriDocNumbered
briDocByExactInlineOnly infoStr ast = do
  let exactPrinted = T.pack $ ExactPrint.exactPrint ast
  fallbackMode <-
    confUnpack . _econf_ExactPrintFallback . _conf_errorHandling <$> mAsk
  let
    exactPrintNode t = allocateNode $ BDExternal False t
  let
    errorAction = do
      mTell [ErrorUnknownNode infoStr (locA (getLoc ast)) (pretty ast)]
      docLitS "{- BRITTANY ERROR UNHANDLED SYNTACTICAL CONSTRUCT -}"
  case (fallbackMode, T.lines exactPrinted) of
    (ExactPrintFallbackModeNever, _) -> errorAction
    (_, [t]) -> exactPrintNode
      (T.dropWhile isSpace . T.dropWhileEnd isSpace $ t)
    (ExactPrintFallbackModeRisky, _) -> exactPrintNode exactPrinted
    _ -> errorAction

rdrNameToText :: RdrName -> Text
-- rdrNameToText = T.pack . show . flip runSDoc unsafeGlobalDynFlags . ppr
rdrNameToText (Unqual occname) = T.pack $ occNameString occname
rdrNameToText (Qual mname occname) =
  T.pack $ moduleNameString mname ++ "." ++ occNameString occname
rdrNameToText (Orig modul occname) =
  T.pack $ moduleNameString (moduleName modul) ++ occNameString occname
rdrNameToText (Exact name) = T.pack $ getOccString name

lrdrNameToText :: GenLocated l RdrName -> Text
lrdrNameToText (L _ n) = rdrNameToText n

nameAdornment :: NameAnn -> Maybe NameAdornment
nameAdornment = \case
  NameAnn{nann_adornment}       -> Just nann_adornment
  NameAnnCommas{nann_adornment} -> Just nann_adornment
  NameAnnOnly{nann_adornment}   -> Just nann_adornment
  NameAnnRArrow{}               -> Nothing
  NameAnnQuote{}                -> Nothing
  NameAnnTrailing{}             -> Nothing
  NameAnnBars{nann_adornment}   -> Just nann_adornment

lrdrNameToTextAnnGen
  :: (Text -> Text)
  -> LocatedN RdrName
  -> Text
lrdrNameToTextAnnGen f (L a n) =
  case (n, nameAdornment (anns (ann a))) of
    (Exact{}, _)
      | t == T.pack "()" -> t
    (_, Just NameBackquotes) -> T.pack "`" <> t <> T.pack "`"
    (_, Just NameParensHash) -> T.pack "(#" <> t <> T.pack "#)"
    (_, Just NameSquare)     -> T.pack "[" <> t <> T.pack "]"
    (_, Just NameParens)     -> T.pack "(" <> t <> T.pack ")"
    _                        -> t
  where
    t :: Text
    t = f $ rdrNameToText n

lrdrNameToTextAnn
  :: LocatedN RdrName
  -> Text
lrdrNameToTextAnn = lrdrNameToTextAnnGen id

isDataTypeEquality :: Text -> Bool
isDataTypeEquality = (== T.pack "Data.Type.Equality~")

-- rraaaahhh special casing rraaahhhhhh
specialCaseDataTypeEquality :: Text -> Text
specialCaseDataTypeEquality x
  | isDataTypeEquality x = T.pack "~"
  | otherwise            = x

lrdrNameToTextAnnTypeEqualityIsSpecial
  :: LocatedN RdrName
  -> Text
lrdrNameToTextAnnTypeEqualityIsSpecial =
  lrdrNameToTextAnnGen specialCaseDataTypeEquality

extractAllComments
  :: LocatedAn ann a -> [LEpaComment]
extractAllComments x =
  priorComments comments ++ getFollowingComments comments
  where
    comments = epAnnComments $ ann $ getLoc x

extractFollowingComments
  :: LocatedAn ann a -> [LEpaComment]
extractFollowingComments = getFollowingComments . epAnnComments . ann . getLoc

-- | True if there are any comments that are
-- a) connected to any node below (in AST sense) the given node AND
-- b) after (in source code order) the node.
hasAnyCommentsBelow :: (Data ann, Data ast) => LocatedAn ann ast -> Bool
hasAnyCommentsBelow ast@(L l _)
  = getAny
  $ astFoldConnectedComments ast
  $ \(c :: Comment) -> Any $ GHC.anchor (ExactPrint.commentAnchor c) > ExactPrint.Utils.rs (locA l)

hasCommentsBetween
  :: Occurrences AddEpAnn ann
  => LocatedAn ann ast
  -> AnnKeywordId
  -> AnnKeywordId
  -> Bool
hasCommentsBetween ast leftKey rightKey = do
  case (epaLocationRealSrcSpan <$> findAnn leftKey, epaLocationRealSrcSpan <$> findAnn rightKey) of
    (Just left, Just right) ->
      any (between (ss2posEnd left) (ss2pos right) . ss2pos . GHC.anchor . ExactPrint.commentAnchor . ExactPrint.Utils.tokComment) $ extractAllComments ast
    _ -> False
  where
    findAnn :: AnnKeywordId -> Maybe EpaLocation
    findAnn target = Monoid.getFirst $ foldAllOccurrences
      (\(AddEpAnn kw loc) -> if kw == target then Monoid.First (Just loc) else mempty)
      (getLoc ast)

    between :: Ord a => a -> a -> a -> Bool
    between l r x = l <= x && x <= r

-- | True if there are any comments that are connected to any node below (in AST
--   sense) the given node
hasAnyCommentsConnected :: (Data ann, Data ast) => LocatedAn ann ast -> Bool
hasAnyCommentsConnected ast =
  getAny $ astFoldConnectedComments ast (const (Any True))

-- | True if there are any regular comments connected to any node below (in AST
--   sense) the given node
hasAnyRegularCommentsConnected :: (Data ann, Data ast) => LocatedAn ann ast -> Bool
hasAnyRegularCommentsConnected ast=
  getAny $ astFoldConnectedComments ast (Any . isRegularComment)

-- | Regular comments are comments that are actually "source code comments",
-- i.e. things that start with "--" or "{-". In contrast to comment-annotations
-- used by ghc-exactprint for capturing symbols (and their exact positioning).
--
-- Only the type instance layouter makes use of this filter currently, but
-- it might make sense to apply it more aggressively or make it the default -
-- I believe that most of the time we branch on the existence of comments, we
-- only care about "regular" comments. We simply did not need the distinction
-- because "irregular" comments are not that common outside of type/data decls.
isRegularComment :: ExactPrint.Comment -> Bool
isRegularComment = isNothing . ExactPrint.Types.commentOrigin

astFoldConnectedComments
  :: forall ann ast a. (Data ann, Data ast, Monoid a)
  => LocatedAn ann ast
  -> (Comment -> a)
  -> a
astFoldConnectedComments ast f = SYB.everything (<>) (SYB.mkQ mempty getComment) ast
  where
    getComment :: LEpaComment -> a
    getComment = f . ExactPrint.Utils.tokComment


hasAnyRegularCommentsRest :: LocatedAn ann ast -> Bool
hasAnyRegularCommentsRest =
  any (isRegularComment . ExactPrint.Utils.tokComment) . extractFollowingComments

hasAnnKeywordComment
  :: LocatedAn ann ast -> AnnKeywordId -> Bool
hasAnnKeywordComment ast annKeyword =
  any hasK $ extractAllComments ast
  where
    hasK :: LEpaComment -> Bool
    hasK = (== Just annKeyword) . ExactPrint.Types.commentOrigin . ExactPrint.Utils.tokComment

hasAnnKeyword
  :: Occurrences AnnKeywordId ann
  => LocatedAn ann a
  -> AnnKeywordId
  -> Bool
hasAnnKeyword ast annKeyword =
  getAny $ foldAllOccurrences (Any . (== annKeyword)) $ getLoc ast

-- new BriDoc stuff

allocateNode
  :: MonadMultiState NodeAllocIndex m => BriDocF BriDocNumbered -> m BriDocNumbered
allocateNode bd = do
  i <- allocNodeIndex
  pure $ i :< bd

allocNodeIndex :: MonadMultiState NodeAllocIndex m => m Int
allocNodeIndex = do
  NodeAllocIndex i <- mGet
  mSet $ NodeAllocIndex (i + 1)
  pure i

docEmpty :: ToBriDocM BriDocNumbered
docEmpty = allocateNode BDEmpty

docLit :: Text -> ToBriDocM BriDocNumbered
docLit = allocateNode . BDLit

docLitS :: String -> ToBriDocM BriDocNumbered
docLitS = docLit . T.pack

docExt
  :: ExactPrint (LocatedAn ann ast)
  => (Text -> Text)
  -> LocatedAn ann ast
  -> Bool
  -> ToBriDocM BriDocNumbered
docExt f x shouldAddComment = allocateNode $ BDExternal
  shouldAddComment
  (f $ T.pack $ ExactPrint.exactPrint x)

docAlt :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docAlt l = allocateNode . BDAlt =<< sequence l

newtype CollectAltM a = CollectAltM (Writer [ToBriDocM BriDocNumbered] a)
  deriving (Functor, Applicative, Monad)

addAlternativeCond :: Bool -> ToBriDocM BriDocNumbered -> CollectAltM ()
addAlternativeCond cond doc = when cond (addAlternative doc)

addAlternative :: ToBriDocM BriDocNumbered -> CollectAltM ()
addAlternative = CollectAltM . tell . (: [])

runFilteredAlternative :: CollectAltM () -> ToBriDocM BriDocNumbered
runFilteredAlternative (CollectAltM action) = docAlt $ execWriter action

docSeq :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docSeq [] = docEmpty
docSeq l  = allocateNode . BDSeq =<< sequence l

docLines :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docLines l = allocateNode . BDLines =<< sequence l

docCols :: ColSig -> [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docCols sig l = allocateNode . BDCols sig =<< sequence l

docAddBaseY :: BrIndent -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAddBaseY ind bdm = allocateNode . BDAddBaseY ind =<< bdm

docSetBaseY :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetBaseY bdm = do
  bd <- bdm
  -- the order here is important so that these two nodes can be treated
  -- properly over at `transformAlts`.
  n1 <- allocateNode $ BDBaseYPushCur bd
  n2 <- allocateNode $ BDBaseYPop n1
  pure n2

docSetIndentLevel :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetIndentLevel bdm = do
  bd <- bdm
  n1 <- allocateNode $ BDIndentLevelPushCur bd
  n2 <- allocateNode $ BDIndentLevelPop n1
  pure n2

docSetBaseAndIndent :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetBaseAndIndent = docSetBaseY . docSetIndentLevel

docSeparator :: ToBriDocM BriDocNumbered
docSeparator = allocateNode BDSeparator

docAnnotationKW
  :: Maybe AnnKeywordId
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docAnnotationKW kw bdm =
  allocateNode . BDAnnotationKW kw =<< bdm

docMoveToKWDP
  :: AnnKeywordId
  -> Bool
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docMoveToKWDP kw shouldRestoreIndent bdm =
  allocateNode . BDMoveToKWDP kw shouldRestoreIndent =<< bdm

docNonBottomSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docNonBottomSpacing bdm = allocateNode . BDNonBottomSpacing False =<< bdm

docNonBottomSpacingS :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docNonBottomSpacingS bdm = allocateNode . BDNonBottomSpacing True =<< bdm

docSetParSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetParSpacing bdm = allocateNode . BDSetParSpacing =<< bdm

docForceParSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceParSpacing bdm = allocateNode . BDForceParSpacing =<< bdm

docDebug :: String -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docDebug s bdm = allocateNode . BDDebug s =<< bdm

appSep :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
appSep x = docSeq [x, docSeparator]

docCommaSep :: ToBriDocM BriDocNumbered
docCommaSep = appSep $ docLitS ","

docParenLSep :: ToBriDocM BriDocNumbered
docParenLSep = appSep docParenL

-- TODO: we don't make consistent use of these (yet). However, I think the
-- most readable approach overall might be something else: define
-- `lit = docLit . T.pack` and `prepSep = docSeq [docSeparator, x]`.
-- I think those two would make the usage most readable.
-- lit "("  and  appSep (lit "(")  are understandable and short without
-- introducing a new top-level binding for all types of parentheses.
docParenL :: ToBriDocM BriDocNumbered
docParenL = docLitS "("

docParenR :: ToBriDocM BriDocNumbered
docParenR = docLitS ")"

docParenHashLSep :: ToBriDocM BriDocNumbered
docParenHashLSep = docSeq [docLitS "(#", docSeparator]

docParenHashRSep :: ToBriDocM BriDocNumbered
docParenHashRSep = docSeq [docSeparator, docLitS "#)"]

docBracketL :: ToBriDocM BriDocNumbered
docBracketL = docLitS "["

docBracketR :: ToBriDocM BriDocNumbered
docBracketR = docLitS "]"

docTick :: ToBriDocM BriDocNumbered
docTick = docLitS "'"

docNodeAnnKW
  :: LocatedAn ann ast
  -> Maybe AnnKeywordId
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docNodeAnnKW _ast kw bdm =
  docAnnotationKW kw bdm

docNodeMoveToKWDP
  :: LocatedAn ann ast
  -> AnnKeywordId
  -> Bool
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docNodeMoveToKWDP _ast kw shouldRestoreIndent bdm =
  docMoveToKWDP kw shouldRestoreIndent bdm

class DocWrapable a where
  docWrapNodeAround :: LocatedAn ann ast -> a -> a
  docWrapNodeBefore :: LocatedAn ann ast -> a -> a
  docWrapNodeAfter  :: LocatedAn ann ast -> a -> a

forgetAnn :: SrcSpanAnn' (EpAnn a) -> EpAnn ()
forgetAnn = void . ann

wrapBefore :: EpAnn () -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
wrapBefore epann bdm = do
  bd <- bdm
  i  <- allocNodeIndex
  pure $ i :< BDAnnotationBefore epann bd

wrapAfter :: EpAnn () -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
wrapAfter epann bdm = do
  bd <- bdm
  i  <- allocNodeIndex
  pure $ i :< BDAnnotationAfter epann bd

instance DocWrapable (ToBriDocM BriDocNumbered) where
  docWrapNodeAround (L ann _ast) bdm = do
    bd <- bdm
    i1 <- allocNodeIndex
    -- i2 <- allocNodeIndex
    pure
      $ (i1 :<)
      $ BDAnnotationBefore (forgetAnn ann)
      --  $ (i2,)
      --  $ BDAnnotationAfter
      $ bd
  docWrapNodeBefore (L ann _ast) =
    wrapBefore (forgetAnn ann)
  docWrapNodeAfter (L ann _ast) =
    wrapAfter (forgetAnn ann)

instance DocWrapable (ToBriDocM a) => DocWrapable [ToBriDocM a] where
  docWrapNodeAround ast bdms = case bdms of
    []   -> []
    [bd] -> [docWrapNodeAround ast bd]
    bd1 : bdR | bdN : bdM <- reverse bdR ->
      [docWrapNodeBefore ast bd1] ++ reverse bdM ++ [docWrapNodeAfter ast bdN]
    _    -> error "cannot happen (TM)"
  docWrapNodeBefore ast bdms = case bdms of
    []        -> []
    [bd]      -> [docWrapNodeBefore ast bd]
    bd1 : bdR -> docWrapNodeBefore ast bd1 : bdR
  docWrapNodeAfter ast bdms = case reverse bdms of
    []        -> []
    bdN : bdR -> reverse $ docWrapNodeAfter ast bdN : bdR

instance DocWrapable (ToBriDocM a) => DocWrapable (ToBriDocM [a]) where
  docWrapNodeAround ast bdsm = do
    bds <- bdsm
    case bds of
      [] -> pure [] -- TODO: this might be bad. maybe. then again, not really. well.
      [bd] -> do
        bd' <- docWrapNodeAround ast (pure bd)
        pure [bd']
      bd1 : bdR | bdN : bdM <- reverse bdR -> do
        bd1' <- docWrapNodeBefore ast (pure bd1)
        bdN' <- docWrapNodeAfter ast (pure bdN)
        pure $ [bd1'] ++ reverse bdM ++ [bdN']
      _ -> error "cannot happen (TM)"
  docWrapNodeBefore ast bdsm = do
    bds <- bdsm
    case bds of
      []          -> pure []
      (bd1 : bdR) -> do
        bd1' <- docWrapNodeBefore ast (pure bd1)
        pure (bd1' : bdR)
  docWrapNodeAfter ast bdsm = do
    bds <- bdsm
    case reverse bds of
      []        -> pure []
      bdN : bdR -> do
        bdN' <- docWrapNodeAfter ast (pure bdN)
        pure $ reverse (bdN' : bdR)

instance DocWrapable (ToBriDocM a) => DocWrapable (ToBriDocM (Seq a)) where
  docWrapNodeAround ast bdsm = do
    bds <- bdsm
    case Seq.viewl bds of
      Seq.EmptyL -> pure Seq.empty -- TODO: this might be bad. maybe. then again, not really. well.
      bd1 Seq.:< rest -> case Seq.viewr rest of
        Seq.EmptyR -> do
          bd1' <- docWrapNodeAround ast (pure bd1)
          pure $ Seq.singleton bd1'
        bdM Seq.:> bdN -> do
          bd1' <- docWrapNodeBefore ast (pure bd1)
          bdN' <- docWrapNodeAfter ast (pure bdN)
          pure $ (bd1' Seq.<| bdM) Seq.|> bdN'
  docWrapNodeBefore ast bdsm = do
    bds <- bdsm
    case Seq.viewl bds of
      Seq.EmptyL -> pure Seq.empty
      bd1 Seq.:< bdR -> do
        bd1' <- docWrapNodeBefore ast (pure bd1)
        pure $ bd1' Seq.<| bdR
  docWrapNodeAfter ast bdsm = do
    bds <- bdsm
    case Seq.viewr bds of
      Seq.EmptyR -> pure Seq.empty
      bdR Seq.:> bdN -> do
        bdN' <- docWrapNodeAfter ast (pure bdN)
        pure $ bdR Seq.|> bdN'

instance DocWrapable (ToBriDocM ([BriDocNumbered], BriDocNumbered, a)) where
  docWrapNodeAround ast stuffM = do
    (bds, bd, x) <- stuffM
    if null bds
      then do
        bd' <- docWrapNodeAround ast (pure bd)
        pure (bds, bd', x)
      else do
        bds' <- docWrapNodeBefore ast (pure bds)
        bd' <- docWrapNodeAfter ast (pure bd)
        pure (bds', bd', x)
  docWrapNodeBefore ast stuffM = do
    (bds, bd, x) <- stuffM
    bds' <- docWrapNodeBefore ast (pure bds)
    pure (bds', bd, x)
  docWrapNodeAfter ast stuffM = do
    (bds, bd, x) <- stuffM
    bd' <- docWrapNodeAfter ast (pure bd)
    pure (bds, bd', x)



docPar
  :: ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docPar lineM indentedM = do
  line     <- lineM
  indented <- indentedM
  allocateNode $ BDPar BrIndentNone line indented

docForceSingleline :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceSingleline bdm = allocateNode . BDForceSingleline =<< bdm

docForceMultiline :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceMultiline bdm = allocateNode . BDForceMultiline =<< bdm

docEnsureIndent
  :: BrIndent -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docEnsureIndent ind mbd = mbd >>= \bd -> allocateNode $ BDEnsureIndent ind bd

unknownNodeError
  :: (Pretty ann, Pretty ast)
  => String
  -> LocatedAn ann ast
  -> ToBriDocM BriDocNumbered
unknownNodeError infoStr ast = do
  mTell [ErrorUnknownNode infoStr (locA (getLoc ast)) (pretty ast)]
  docLitS "{- BRITTANY ERROR UNHANDLED SYNTACTICAL CONSTRUCT -}"

spacifyDocs :: [ToBriDocM BriDocNumbered] -> [ToBriDocM BriDocNumbered]
spacifyDocs [] = []
spacifyDocs ds = fmap appSep (L.init ds) ++ [L.last ds]

briDocMToPPM :: ToBriDocM a -> PPMLocal a
briDocMToPPM m = do
  (x, errs, debugs) <- briDocMToPPMInner m
  mTell debugs
  mTell errs
  pure x

briDocMToPPMInner :: ToBriDocM a -> PPMLocal (a, [BrittanyError], Seq String)
briDocMToPPMInner m = do
  readers <- MultiRWSS.mGetRawR
  let
    ((x, errs), debugs) =
      runIdentity
        $ MultiRWSS.runMultiRWSTNil
        $ MultiRWSS.withMultiStateA (NodeAllocIndex 1)
        $ MultiRWSS.withMultiReaders readers
        $ MultiRWSS.withMultiWriterAW
        $ MultiRWSS.withMultiWriterAW
        $ m
  pure (x, errs, debugs)

docSharedWrapper :: Monad m => (x -> m y) -> x -> m (m y)
docSharedWrapper f x = pure <$> f x
