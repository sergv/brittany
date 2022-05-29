{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Language.Haskell.Brittany.Internal.LayouterBasics where

import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Control.Monad.Writer.Strict as Writer
import qualified Data.Char as Char
import Data.Coerce (coerce)
import Data.Data (Data)
import qualified Data.Generics as SYB
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid as Monoid
import Data.Occurrences
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text.Builder
import DataTreePrint
import GHC (GenLocated(L), Located, moduleName, moduleNameString, getFollowingComments)
import qualified GHC.OldList as List
import GHC.Parser.Annotation (AnnKeywordId(..))
import GHC.Parser.Annotation as GHC
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName(..))
import GHC.Types.SrcLoc (getLoc, unLoc)
import qualified GHC.Types.SrcLoc as GHC
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.ExactPrintUtils
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils
import Language.Haskell.GHC.ExactPrint (ExactPrint(..), exactPrint)
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types

import Language.Haskell.GHC.ExactPrint.Utils (ss2posEnd, ss2pos)
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrint.Utils

processDefault
  :: ( MonadMultiWriter Text.Builder.Builder m
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
    str  -> mTell $ Text.Builder.fromString str

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
  :: Data ast
  => ExactPrint (LocatedAn ann ast)
  => String
  -> LocatedAn ann ast
  -> ToBriDocM BriDocNumbered
briDocByExactInlineOnly infoStr ast = do
  let exactPrinted = Text.pack $ ExactPrint.exactPrint ast
  fallbackMode <-
    mAsk <&> _conf_errorHandling .> _econf_ExactPrintFallback .> confUnpack
  let
    exactPrintNode t = allocateNode $ BDFExternal False t
  let
    errorAction = do
      mTell [ErrorUnknownNode infoStr ast]
      docLitS "{- BRITTANY ERROR UNHANDLED SYNTACTICAL CONSTRUCT -}"
  case (fallbackMode, Text.lines exactPrinted) of
    (ExactPrintFallbackModeNever, _) -> errorAction
    (_, [t]) -> exactPrintNode
      (Text.dropWhile Char.isSpace . Text.dropWhileEnd Char.isSpace $ t)
    (ExactPrintFallbackModeRisky, _) -> exactPrintNode exactPrinted
    _ -> errorAction

rdrNameToText :: RdrName -> Text
-- rdrNameToText = Text.pack . show . flip runSDoc unsafeGlobalDynFlags . ppr
rdrNameToText (Unqual occname) = Text.pack $ occNameString occname
rdrNameToText (Qual mname occname) =
  Text.pack $ moduleNameString mname ++ "." ++ occNameString occname
rdrNameToText (Orig modul occname) =
  Text.pack $ moduleNameString (moduleName modul) ++ occNameString occname
rdrNameToText (Exact name) = Text.pack $ getOccString name

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

lrdrNameToTextAnnGen
  :: (Text -> Text)
  -> LocatedN RdrName
  -> Text
lrdrNameToTextAnnGen f (L a n) =
  case (n, nameAdornment (anns (ann a))) of
    (Exact{}, _)
      | t == Text.pack "()" -> t
    (_, Just NameBackquotes) -> Text.pack "`" <> t <> Text.pack "`"
    (_, Just NameParensHash) -> Text.pack "(#" <> t <> Text.pack "#)"
    (_, Just NameSquare)     -> Text.pack "[" <> t <> Text.pack "]"
    (_, Just NameParens)     -> Text.pack "(" <> t <> Text.pack ")"
    _                        -> t
  where
    t :: Text
    t = f $ rdrNameToText n

lrdrNameToTextAnn
  :: LocatedN RdrName
  -> Text
lrdrNameToTextAnn = lrdrNameToTextAnnGen id

isDataTypeEquality :: Text -> Bool
isDataTypeEquality = (== Text.pack "Data.Type.Equality~")

-- rraaaahhh special casing rraaahhhhhh
specialCaseDataTypeEquality :: Text -> Text
specialCaseDataTypeEquality x
  | isDataTypeEquality x = Text.pack "~"
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
  case (epaLocationRealSrcSpan <$> find leftKey, epaLocationRealSrcSpan <$> find rightKey) of
    (Just left, Just right) ->
      any (between (ss2posEnd left) (ss2pos right) . ss2pos . GHC.anchor . ExactPrint.commentAnchor . ExactPrint.Utils.tokComment) $ extractAllComments ast
    _ -> False
  where
    find :: AnnKeywordId -> Maybe EpaLocation
    find target = Monoid.getFirst $ foldAllOccurrences
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
  :: MonadMultiState NodeAllocIndex m => BriDocFInt -> m BriDocNumbered
allocateNode bd = do
  i <- allocNodeIndex
  return (i, bd)

allocNodeIndex :: MonadMultiState NodeAllocIndex m => m Int
allocNodeIndex = do
  NodeAllocIndex i <- mGet
  mSet $ NodeAllocIndex (i + 1)
  return i

docEmpty :: ToBriDocM BriDocNumbered
docEmpty = allocateNode BDFEmpty

docLit :: Text -> ToBriDocM BriDocNumbered
docLit = allocateNode . BDFLit

docLitS :: String -> ToBriDocM BriDocNumbered
docLitS = docLit . Text.pack

docExt
  :: ExactPrint (LocatedAn ann ast)
  => (Text -> Text)
  -> LocatedAn ann ast
  -> Bool
  -> ToBriDocM BriDocNumbered
docExt f x shouldAddComment = allocateNode $ BDFExternal
  shouldAddComment
  (f $ Text.pack $ ExactPrint.exactPrint x)

docAlt :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docAlt l = allocateNode . BDFAlt =<< sequence l


newtype CollectAltM a = CollectAltM (Writer.Writer [ToBriDocM BriDocNumbered] a)
  deriving (Functor, Applicative, Monad)

addAlternativeCond :: Bool -> ToBriDocM BriDocNumbered -> CollectAltM ()
addAlternativeCond cond doc = when cond (addAlternative doc)

addAlternative :: ToBriDocM BriDocNumbered -> CollectAltM ()
addAlternative = CollectAltM . Writer.tell . (: [])

runFilteredAlternative :: CollectAltM () -> ToBriDocM BriDocNumbered
runFilteredAlternative (CollectAltM action) = docAlt $ Writer.execWriter action


docSeq :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docSeq [] = docEmpty
docSeq l = allocateNode . BDFSeq =<< sequence l

docLines :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docLines l = allocateNode . BDFLines =<< sequence l

docCols :: ColSig -> [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docCols sig l = allocateNode . BDFCols sig =<< sequence l

docAddBaseY :: BrIndent -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAddBaseY ind bdm = allocateNode . BDFAddBaseY ind =<< bdm

docSetBaseY :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetBaseY bdm = do
  bd <- bdm
  -- the order here is important so that these two nodes can be treated
  -- properly over at `transformAlts`.
  n1 <- allocateNode $ BDFBaseYPushCur bd
  n2 <- allocateNode $ BDFBaseYPop n1
  return n2

docSetIndentLevel :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetIndentLevel bdm = do
  bd <- bdm
  n1 <- allocateNode $ BDFIndentLevelPushCur bd
  n2 <- allocateNode $ BDFIndentLevelPop n1
  return n2

docSetBaseAndIndent :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetBaseAndIndent = docSetBaseY . docSetIndentLevel

docSeparator :: ToBriDocM BriDocNumbered
docSeparator = allocateNode BDFSeparator

docAnnotationKW
  :: Maybe AnnKeywordId
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docAnnotationKW kw bdm =
  allocateNode . BDFAnnotationKW kw =<< bdm

docMoveToKWDP
  :: AnnKeywordId
  -> Bool
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docMoveToKWDP kw shouldRestoreIndent bdm =
  allocateNode . BDFMoveToKWDP kw shouldRestoreIndent =<< bdm

docAnnotationRest
  :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAnnotationRest bdm = allocateNode . BDFAnnotationAfter =<< bdm

docNonBottomSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docNonBottomSpacing bdm = allocateNode . BDFNonBottomSpacing False =<< bdm

docNonBottomSpacingS :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docNonBottomSpacingS bdm = allocateNode . BDFNonBottomSpacing True =<< bdm

docSetParSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetParSpacing bdm = allocateNode . BDFSetParSpacing =<< bdm

docForceParSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceParSpacing bdm = allocateNode . BDFForceParSpacing =<< bdm

docDebug :: String -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docDebug s bdm = allocateNode . BDFDebug s =<< bdm

appSep :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
appSep x = docSeq [x, docSeparator]

docCommaSep :: ToBriDocM BriDocNumbered
docCommaSep = appSep $ docLitS ","

docParenLSep :: ToBriDocM BriDocNumbered
docParenLSep = appSep docParenL

-- TODO: we don't make consistent use of these (yet). However, I think the
-- most readable approach overall might be something else: define
-- `lit = docLit . Text.pack` and `prepSep = docSeq [docSeparator, x]`.
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
  docWrapNodeAround      :: LocatedAn ann ast -> a -> a
  docWrapNodeBefore :: LocatedAn ann ast -> a -> a
  docWrapNodeAfter  :: LocatedAn ann ast -> a -> a

forgetAnn :: SrcSpanAnn' (EpAnn a) -> EpAnn ()
forgetAnn = void . ann

wrapBefore :: EpAnn () -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
wrapBefore epann bdm = do
  bd <- bdm
  i  <- allocNodeIndex
  pure (i, BDFAnnotationBefore epann bd)

instance DocWrapable (ToBriDocM BriDocNumbered) where
  docWrapNodeAround (L ann _ast) bdm = do
    bd <- bdm
    i1 <- allocNodeIndex
    i2 <- allocNodeIndex
    pure
      $ (i1,)
      $ BDFAnnotationBefore (forgetAnn ann)
      $ (i2,)
      $ BDFAnnotationAfter
      $ bd
  docWrapNodeBefore (L ann _ast) =
    wrapBefore (forgetAnn ann)
  docWrapNodeAfter (L ann _ast) bdm = do
    bd <- bdm
    i2 <- allocNodeIndex
    pure $ (,) i2 $ BDFAnnotationAfter bd

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
      Seq.EmptyL -> return Seq.empty -- TODO: this might be bad. maybe. then again, not really. well.
      bd1 Seq.:< rest -> case Seq.viewr rest of
        Seq.EmptyR -> do
          bd1' <- docWrapNodeAround ast (return bd1)
          return $ Seq.singleton bd1'
        bdM Seq.:> bdN -> do
          bd1' <- docWrapNodeBefore ast (return bd1)
          bdN' <- docWrapNodeAfter ast (return bdN)
          return $ (bd1' Seq.<| bdM) Seq.|> bdN'
  docWrapNodeBefore ast bdsm = do
    bds <- bdsm
    case Seq.viewl bds of
      Seq.EmptyL -> return Seq.empty
      bd1 Seq.:< bdR -> do
        bd1' <- docWrapNodeBefore ast (return bd1)
        return $ bd1' Seq.<| bdR
  docWrapNodeAfter ast bdsm = do
    bds <- bdsm
    case Seq.viewr bds of
      Seq.EmptyR -> return Seq.empty
      bdR Seq.:> bdN -> do
        bdN' <- docWrapNodeAfter ast (return bdN)
        return $ bdR Seq.|> bdN'

instance DocWrapable (ToBriDocM ([BriDocNumbered], BriDocNumbered, a)) where
  docWrapNodeAround ast stuffM = do
    (bds, bd, x) <- stuffM
    if null bds
      then do
        bd' <- docWrapNodeAround ast (return bd)
        return (bds, bd', x)
      else do
        bds' <- docWrapNodeBefore ast (return bds)
        bd' <- docWrapNodeAfter ast (return bd)
        return (bds', bd', x)
  docWrapNodeBefore ast stuffM = do
    (bds, bd, x) <- stuffM
    bds' <- docWrapNodeBefore ast (return bds)
    return (bds', bd, x)
  docWrapNodeAfter ast stuffM = do
    (bds, bd, x) <- stuffM
    bd' <- docWrapNodeAfter ast (return bd)
    return (bds, bd', x)



docPar
  :: ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docPar lineM indentedM = do
  line <- lineM
  indented <- indentedM
  allocateNode $ BDFPar BrIndentNone line indented

docForceSingleline :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceSingleline bdm = allocateNode . BDFForceSingleline =<< bdm

docForceMultiline :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceMultiline bdm = allocateNode . BDFForceMultiline =<< bdm

docEnsureIndent
  :: BrIndent -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docEnsureIndent ind mbd = mbd >>= \bd -> allocateNode $ BDFEnsureIndent ind bd

unknownNodeError
  :: Data ast
  => String
  -> LocatedAn ann ast
  -> ToBriDocM BriDocNumbered
unknownNodeError infoStr ast = do
  mTell [ErrorUnknownNode infoStr ast]
  docLitS "{- BRITTANY ERROR UNHANDLED SYNTACTICAL CONSTRUCT -}"

spacifyDocs :: [ToBriDocM BriDocNumbered] -> [ToBriDocM BriDocNumbered]
spacifyDocs [] = []
spacifyDocs ds = fmap appSep (List.init ds) ++ [List.last ds]

briDocMToPPM :: ToBriDocM a -> PPMLocal a
briDocMToPPM m = do
  (x, errs, debugs) <- briDocMToPPMInner m
  mTell debugs
  mTell errs
  return x

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
