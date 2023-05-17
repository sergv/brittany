{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.Layouters.Decl
  ( layoutDecl
  , layoutLocalBinds
  , layoutPatternBind
  , layoutGrhs
  , layoutPatternBindFinal
  ) where

import Data.Data (Data)
import Data.Foldable qualified
import Data.Functor
import Data.List qualified as L
import Data.Maybe qualified
import Data.Occurrences
import Data.Semigroup qualified as Semigroup
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Traversable
import Prettyprinter (Pretty(..))

import GHC (GenLocated(L))
import GHC.Data.Bag (bagToList, emptyBag)
import GHC.Data.FastString qualified as FastString
import GHC.Hs
import GHC.OldList qualified as List
import GHC.Types.Basic (Activation(..), InlinePragma(..), InlineSpec(..), RuleMatchInfo(..))
import GHC.Types.Fixity (LexicalFixity(..))
import GHC.Types.SrcLoc (SrcSpan, getLoc, unLoc)
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.ExactPrintUtils
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.DataDecl
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import Language.Haskell.Brittany.Internal.Layouters.Pattern
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Stmt
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.GHC.ExactPrint.Utils qualified as ExactPrint

layoutDecl :: LHsDecl GhcPs -> ToBriDocM BriDocNumbered
layoutDecl d@(L loc decl) = do
  case decl of
    SigD NoExtField sig -> withTransformedAnns d $ layoutSig (L loc sig)
    ValD NoExtField bind -> withTransformedAnns d $ layoutBind (L loc bind) >>= \case
      Left ns -> docLines $ pure <$> ns
      Right n -> pure n
    TyClD NoExtField tycl -> withTransformedAnns d $ layoutTyCl (L loc tycl)
    InstD NoExtField (TyFamInstD _ tfid) ->
      withTransformedAnns d $ layoutTyFamInstDecl False d tfid
    InstD NoExtField (ClsInstD _ inst) ->
      withTransformedAnns d $ layoutClsInst (L loc inst)
    _ -> briDocByExactNoComment d

--------------------------------------------------------------------------------
-- Sig
--------------------------------------------------------------------------------

layoutSig :: LSig GhcPs -> ToBriDocM BriDocNumbered
layoutSig lsig@(L _loc sig) = case sig of
  InlineSig _ name (InlinePragma _ spec _arity phaseAct conlike) ->
    docWrapNodeAround lsig $ do
      specStr <- specStringCompat lsig spec
      let nameStr  = lrdrNameToTextAnn name
          phaseStr = case phaseAct of
            NeverActive      -> "" -- not [] - for NOINLINE NeverActive is
                                   -- in fact the default
            AlwaysActive     -> ""
            ActiveBefore _ i -> "[~" ++ show i ++ "] "
            ActiveAfter _ i  -> "[" ++ show i ++ "] "
            FinalActive      -> error "brittany internal error: FinalActive"
          conlikeStr :: String
          conlikeStr = case conlike of
            FunLike -> ""
            ConLike -> "CONLIKE "
      docLit
        $ T.pack ("{-# " ++ specStr ++ conlikeStr ++ phaseStr)
        <> nameStr
        <> " #-}"
  TypeSig _ names (HsWC _ typ) -> layoutNamesAndType Nothing names typ
  ClassOpSig _ False names typ -> layoutNamesAndType Nothing names typ
  PatSynSig _ names typ        -> layoutNamesAndType (Just "pattern") names typ
  _                            -> briDocByExactNoComment lsig -- TODO
  where
    layoutNamesAndType
      :: Maybe String
      -> [LocatedAn NameAnn RdrName]
      -> LHsSigType GhcPs
      -> ToBriDocM BriDocNumbered
    layoutNamesAndType mKeyword names typ = docWrapNodeAround lsig $ do
      let keyDoc      = case mKeyword of
            Just key -> [appSep $ docLitS key]
            Nothing  -> []
          nameStrs    = map lrdrNameToTextAnn names
          nameStr     = Text.intercalate ", " $ nameStrs
          hasComments = hasAnyCommentsBelow lsig
      typeDoc <- docSharedWrapper layoutSigType typ
      shouldBeHanging <- mAsk <&> (_conf_layout >>> _lconfig_hangingTypeSignature >>> confUnpack)
      if shouldBeHanging
      then
        docSeq
          [ appSep
          $ docSeq
          $ keyDoc
          <> [docLit nameStr]
          , docSetBaseY $ docLines
            [ docCols
                ColTyOpPrefix
                [ docLitS ":: "
                , docAddBaseY (BrIndentSpecial 3) typeDoc
                ]
            ]
          ]
      else layoutLhsAndType
        hasComments
        (appSep . docSeq $ keyDoc <> [docLit nameStr])
        "::"
        typeDoc

specStringCompat
  :: MonadMultiWriter [BrittanyError] m => LSig GhcPs -> InlineSpec -> m String
specStringCompat ast = \case
  NoUserInlinePrag -> "" <$ mTell [ErrorUnknownNode "NoUserInlinePrag" (locA (getLoc ast)) (pretty ast)]
  Inline    _      -> pure "INLINE "
  Inlinable _      -> pure "INLINABLE "
  NoInline  _      -> pure "NOINLINE "
  Opaque    _      -> pure "OPAQUE "

_layoutGuardLStmt :: LStmt GhcPs (LHsExpr GhcPs) -> ToBriDocM BriDocNumbered
_layoutGuardLStmt lgstmt@(L _ stmtLR) = docWrapNodeAround lgstmt $ case stmtLR of
  BodyStmt _ body _ _ -> layoutExpr body
  BindStmt _ lPat expr -> do
    patDoc <- docSharedWrapper layoutPat lPat
    expDoc <- docSharedWrapper layoutExpr expr
    docCols
      ColBindStmt
      [ appSep $ colsWrapPat =<< patDoc
      , docSeq [appSep $ docLitS "<-", expDoc]
      ]
  _ -> unknownNodeError "" lgstmt -- TODO

--------------------------------------------------------------------------------
-- HsBind
--------------------------------------------------------------------------------

layoutBind
  :: LHsBindLR GhcPs GhcPs -> ToBriDocM (Either [BriDocNumbered] BriDocNumbered)
layoutBind lbind@(L _ bind) = case bind of
  FunBind _ fId (MG _ lmatches@(L _ matches)) -> do
    let idStr = lrdrNameToTextAnn fId
    binderDoc   <- docLitS "="
    funcPatDocs <- docWrapNodeAround lbind
      $ docWrapNodeAround lmatches
      $ traverse (layoutPatternBind (Just idStr) binderDoc) matches
    pure $ Left $ funcPatDocs
  PatBind _ pat (GRHSs _ grhss whereBinds) -> do
    patDocs     <- colsWrapPat =<< layoutPat pat
    clauseDocs  <- traverse layoutGrhs grhss
    mWhereDocs  <- layoutLocalBinds whereBinds
    binderDoc   <- docLitS "="
    let hasComments = hasAnyCommentsBelow lbind
    fmap Right $ docWrapNodeAround lbind $ layoutPatternBindFinal
      Nothing
      binderDoc
      (Just patDocs)
      clauseDocs
      mWhereDocs
      hasComments
  PatSynBind _ (PSB _ patID lpat rpat dir) ->
    fmap Right $ docWrapNodeAround lbind $ layoutPatSynBind patID lpat dir rpat
  _ -> Right <$> unknownNodeError "" lbind

layoutIPBind :: LIPBind GhcPs -> ToBriDocM BriDocNumbered
layoutIPBind lipbind@(L _ bind) = case bind of
  IPBind _ (L _ (HsIPName name)) expr -> do
    ipName      <- docLitS $ '?' : FastString.unpackFS name
    binderDoc   <- docLitS "="
    exprDoc     <- layoutExpr expr
    let hasComments = hasAnyCommentsBelow lipbind
    layoutPatternBindFinal
      Nothing
      binderDoc
      (Just ipName)
      [([], exprDoc, expr)]
      Nothing
      hasComments

data BagBindOrSig
  = BagBind (LHsBindLR GhcPs GhcPs)
  | BagSig (LSig GhcPs)

bindOrSigtoSrcSpan :: BagBindOrSig -> SrcSpan
bindOrSigtoSrcSpan (BagBind (L l _)) = locA l
bindOrSigtoSrcSpan (BagSig (L l _)) = locA l

layoutLocalBinds
  :: HsLocalBindsLR GhcPs GhcPs -> ToBriDocM (Maybe [BriDocNumbered])
layoutLocalBinds binds = case binds of
  -- HsValBinds (ValBindsIn lhsBindsLR []) ->
  --   Just . (>>= either id pure) . Data.Foldable.toList <$> mapBagM layoutBind lhsBindsLR -- TODO: fix ordering
  -- x@(HsValBinds (ValBindsIn{})) ->
  --   Just . (:[]) <$> unknownNodeError "HsValBinds (ValBindsIn _ (_:_))" x
  HsValBinds _ (ValBinds _ bindlrs sigs) -> do
    let unordered =
          [ BagBind b | b <- Data.Foldable.toList bindlrs ]
          ++ [ BagSig s | s <- sigs ]
        ordered = List.sortOn (ExactPrint.rs . bindOrSigtoSrcSpan) unordered
    docs <- for ordered $ \case
      BagBind b -> either id pure <$> layoutBind b
      BagSig  s -> pure <$> layoutSig s
    pure $ Just $ L.concat docs
--  x@(HsValBinds (ValBindsOut _binds _lsigs)) ->
  HsValBinds _ XValBindsLR{} -> error "brittany internal error: XValBindsLR"
  HsIPBinds _ (IPBinds _ bb) -> Just <$> mapM layoutIPBind bb
  EmptyLocalBinds{} -> pure $ Nothing

-- TODO: we don't need the `LHsExpr GhcPs` anymore, now that there is
-- parSpacing stuff.B
layoutGrhs
  :: LGRHS GhcPs (LHsExpr GhcPs)
  -> ToBriDocM ([BriDocNumbered], BriDocNumbered, LHsExpr GhcPs)
layoutGrhs (L _ (GRHS _ guards body)) = do
  guardDocs <- traverse layoutStmt guards
  bodyDoc   <- layoutExpr body
  pure (guardDocs, bodyDoc, body)

layoutPatternBind
  :: Maybe Text
  -> BriDocNumbered
  -> LMatch GhcPs (LHsExpr GhcPs)
  -> ToBriDocM BriDocNumbered
layoutPatternBind funId binderDoc lmatch@(L _ match) = do
  let pats = m_pats match
  let (GRHSs _ grhss whereBinds) = m_grhss match
  patDocs <- pats `forM` \p -> fmap pure $ colsWrapPat =<< layoutPat p
  let isInfix = isInfixMatch match
  mIdStr <- case match of
    Match _ (FunRhs matchId _ _) _ _ -> pure $ Just $ lrdrNameToTextAnn matchId
    _ -> pure Nothing
  let mIdStr' = fixPatternBindIdentifier match <$> mIdStr
  patDoc <- docWrapNodeBefore lmatch $ case (mIdStr', patDocs) of
    (Just idStr, p1 : p2 : pr) | isInfix -> if null pr
      then docCols
        ColPatternsFuncInfix
        [ appSep $ docForceSingleline p1
        , appSep $ docLit idStr
        , docForceSingleline p2
        ]
      else docCols
        ColPatternsFuncInfix
        ([ docCols
             ColPatterns
             [ docParenL
             , appSep $ docForceSingleline p1
             , appSep $ docLit idStr
             , docForceSingleline p2
             , appSep $ docParenR
             ]
         ]
        ++ spacifyDocs (docForceSingleline <$> pr)
        )
    (Just idStr, []) -> docLit idStr
    (Just idStr, ps) ->
      docCols ColPatternsFuncPrefix
        $ appSep (docLit idStr)
        : (spacifyDocs $ docForceSingleline <$> ps)
    (Nothing, ps) ->
      docCols ColPatterns
        $ (List.intersperse docSeparator $ docForceSingleline <$> ps)
  clauseDocs <- docWrapNodeAfter lmatch $ layoutGrhs `mapM` grhss
  mWhereDocs <- layoutLocalBinds whereBinds
  let alignmentToken = if null pats then Nothing else funId
      hasComments    = hasAnyCommentsBelow lmatch
  layoutPatternBindFinal
    alignmentToken
    binderDoc
    (Just patDoc)
    clauseDocs
    mWhereDocs
    hasComments

fixPatternBindIdentifier :: Match GhcPs (LHsExpr GhcPs) -> Text -> Text
fixPatternBindIdentifier match idStr = go $ m_ctxt match
 where
  go = \case
    (FunRhs _ _ SrcLazy) -> Text.cons '~' idStr
    (FunRhs _ _ SrcStrict) -> Text.cons '!' idStr
    (FunRhs _ _ NoSrcStrict) -> idStr
    (StmtCtxt ctx1) -> goInner ctx1
    _ -> idStr
  -- I have really no idea if this path ever occurs, but better safe than
  -- risking another "drop bangpatterns" bugs.
  goInner = \case
    (PatGuard ctx1) -> go ctx1
    (ParStmtCtxt ctx1) -> goInner ctx1
    (TransStmtCtxt ctx1) -> goInner ctx1
    _ -> idStr

layoutPatternBindFinal
  :: Maybe Text
  -> BriDocNumbered
  -> Maybe BriDocNumbered
  -> [([BriDocNumbered], BriDocNumbered, LHsExpr GhcPs)]
  -> Maybe [BriDocNumbered]
     -- ^ AnnKey for the node that contains the AnnWhere position annotation
  -> Bool
  -> ToBriDocM BriDocNumbered
layoutPatternBindFinal alignmentToken binderDoc mPatDoc clauseDocs mWhereDocs hasComments = do
  let patPartInline  = case mPatDoc of
        Nothing     -> []
        Just patDoc -> [appSep $ docForceSingleline $ pure patDoc]
      patPartParWrap = case mPatDoc of
        Nothing     -> id
        Just patDoc -> docPar (pure patDoc)
  whereIndent <- do
    shouldSpecial <-
      mAsk <&> (_conf_layout >>> _lconfig_indentWhereSpecial >>> confUnpack)
    regularIndentAmount <-
      mAsk <&> (_conf_layout >>> _lconfig_indentAmount >>> confUnpack)
    pure $ if shouldSpecial
      then BrIndentSpecial (max 1 (regularIndentAmount `div` 2))
      else BrIndentRegular
  -- TODO: apart from this, there probably are more nodes below which could
  --       be shared between alternatives.
  wherePartMultiLine :: [ToBriDocM BriDocNumbered] <- case mWhereDocs of
    Nothing -> pure []
    Just [w] -> pure . pure <$> docAlt
      [ docEnsureIndent BrIndentRegular
        $ docSeq
            [ docLitS "where"
            , docSeparator
            , docForceSingleline $ pure w
            ]
      , docMoveToKWDP AnnWhere False
      $ docEnsureIndent whereIndent
      $ docLines
          [ docLitS "where"
          , docEnsureIndent whereIndent
          $ docSetIndentLevel
          $ docNonBottomSpacing
          $ pure w
          ]
      ]
    Just ws ->
      fmap (pure . pure)
        $ docMoveToKWDP AnnWhere False
        $ docEnsureIndent whereIndent
        $ docLines
            [ docLitS "where"
            , docEnsureIndent whereIndent
            $ docSetIndentLevel
            $ docNonBottomSpacing
            $ docLines
            $ pure
            <$> ws
            ]
  let
    singleLineGuardsDoc guards = appSep $ case guards of
      [] -> docEmpty
      [g] -> docSeq
        [appSep $ docLitS "|", docForceSingleline $ pure g]
      gs ->
        docSeq
          $ [appSep $ docLitS "|"]
          ++ (List.intersperse
               docCommaSep
               (docForceSingleline . pure <$> gs)
             )
    wherePart = case mWhereDocs of
      Nothing  -> Just docEmpty
      Just [w] -> Just $ docSeq
        [ docSeparator
        , appSep $ docLitS "where"
        , docSetIndentLevel $ docForceSingleline $ pure w
        ]
      _        -> Nothing

  indentPolicy <- mAsk <&> (_conf_layout >>> _lconfig_indentPolicy >>> confUnpack)

  runFilteredAlternative $ do

    case clauseDocs of
      [(guards, body, _bodyRaw)] -> do
        let guardPart = singleLineGuardsDoc guards
        forM_ wherePart $ \wherePart' ->
          -- one-line solution
          addAlternativeCond (not hasComments) $ docCols
            (ColBindingLine alignmentToken)
            [ docSeq (patPartInline ++ [guardPart])
            , docSeq
              [ appSep $ pure binderDoc
              , docForceSingleline $ pure body
              , wherePart'
              ]
            ]
        -- one-line solution + where in next line(s)
        addAlternativeCond (Data.Maybe.isJust mWhereDocs)
          $ docLines
          $ [ docCols
                (ColBindingLine alignmentToken)
                [ docSeq (patPartInline ++ [guardPart])
                , docSeq
                  [ appSep $ pure binderDoc
                  , docForceParSpacing $ docAddBaseY BrIndentRegular $ pure
                    body
                  ]
                ]
            ]
          ++ wherePartMultiLine
        -- two-line solution + where in next line(s)
        addAlternative
          $ docLines
          $ [ docForceSingleline
              $ docSeq (patPartInline ++ [guardPart, pure binderDoc])
            , docEnsureIndent BrIndentRegular $ docForceSingleline $ pure
              body
            ]
          ++ wherePartMultiLine
        -- pattern and exactly one clause in single line, body as par;
        -- where in following lines
        addAlternative
          $ docLines
          $ [ docCols
                (ColBindingLine alignmentToken)
                [ docSeq (patPartInline ++ [guardPart])
                , docSeq
                  [ appSep $ pure binderDoc
                  , docForceParSpacing $ docAddBaseY BrIndentRegular $ pure
                    body
                  ]
                ]
            ]
           -- , lineMod $ docAlt
           --   [ docSetBaseY $ pure body
           --   , docAddBaseY BrIndentRegular $ pure body
           --   ]
          ++ wherePartMultiLine
        -- pattern and exactly one clause in single line, body in new line.
        addAlternative
          $ docLines
          $ [ docSeq (patPartInline ++ [guardPart, pure binderDoc])
            , docNonBottomSpacing
            $ docEnsureIndent BrIndentRegular
            $ docAddBaseY BrIndentRegular
            $ pure body
            ]
          ++ wherePartMultiLine

      _ -> pure () -- no alternatives exclusively when `length clauseDocs /= 1`

    case mPatDoc of
      Nothing -> pure ()
      Just patDoc ->
        -- multiple clauses added in-paragraph, each in a single line
        -- example: foo | bar = baz
        --              | lll = asd
        addAlternativeCond (indentPolicy == IndentPolicyFree)
          $ docLines
          $ [ docSeq
                [ appSep $ docForceSingleline $ pure patDoc
                , docSetBaseY
                $ docLines
                $ clauseDocs
                <&> \(guardDocs, bodyDoc, _) -> do
                      let guardPart = singleLineGuardsDoc guardDocs
                      -- the docForceSingleline might seems superflous, but it
                      -- helps the alternative resolving impl.
                      docForceSingleline $ docCols
                        ColGuardedBody
                        [ guardPart
                        , docSeq
                          [ appSep $ pure binderDoc
                          , docForceSingleline $ pure bodyDoc
                          -- i am not sure if there is a benefit to using
                          -- docForceParSpacing additionally here:
                          -- , docAddBaseY BrIndentRegular $ pure bodyDoc
                          ]
                        ]
                ]
            ]
          ++ wherePartMultiLine
    -- multiple clauses, each in a separate, single line
    addAlternative
      $ docLines
      $ [ docAddBaseY BrIndentRegular
          $ patPartParWrap
          $ docLines
          $ map docSetBaseY
          $ clauseDocs
          <&> \(guardDocs, bodyDoc, _) -> do
                let guardPart = singleLineGuardsDoc guardDocs
                -- the docForceSingleline might seems superflous, but it
                -- helps the alternative resolving impl.
                docForceSingleline $ docCols
                  ColGuardedBody
                  [ guardPart
                  , docSeq
                    [ appSep $ pure binderDoc
                    , docForceSingleline $ pure bodyDoc
                    -- i am not sure if there is a benefit to using
                    -- docForceParSpacing additionally here:
                    -- , docAddBaseY BrIndentRegular $ pure bodyDoc
                    ]
                  ]
        ]
      ++ wherePartMultiLine
    -- multiple clauses, each with the guard(s) in a single line, body
    -- as a paragraph
    addAlternative
      $ docLines
      $ [ docAddBaseY BrIndentRegular
          $ patPartParWrap
          $ docLines
          $ map docSetBaseY
          $ clauseDocs
          <&> \(guardDocs, bodyDoc, _) ->
                docSeq
                  $ (case guardDocs of
                      [] -> []
                      [g] ->
                        [ docForceSingleline $ docSeq
                            [appSep $ docLitS "|", pure g]
                        ]
                      gs ->
                        [ docForceSingleline
                            $ docSeq
                            $ [appSep $ docLitS "|"]
                            ++ List.intersperse docCommaSep (pure <$> gs)
                        ]
                    )
                  ++ [ docSeparator
                     , docCols
                       ColOpPrefix
                       [ appSep $ pure binderDoc
                       , docAddBaseY BrIndentRegular
                       $ docForceParSpacing
                       $ pure bodyDoc
                       ]
                     ]
        ]
      ++ wherePartMultiLine
    -- multiple clauses, each with the guard(s) in a single line, body
    -- in a new line as a paragraph
    addAlternative
      $ docLines
      $ [ docAddBaseY BrIndentRegular
          $ patPartParWrap
          $ docLines
          $ map docSetBaseY
          $ clauseDocs
          >>= \(guardDocs, bodyDoc, _) ->
                (case guardDocs of
                    [] -> []
                    [g] ->
                      [ docForceSingleline
                          $ docSeq [appSep $ docLitS "|", pure g]
                      ]
                    gs ->
                      [ docForceSingleline
                          $ docSeq
                          $ [appSep $ docLitS "|"]
                          ++ List.intersperse docCommaSep (pure <$> gs)
                      ]
                  )
                  ++ [ docCols
                         ColOpPrefix
                         [ appSep $ pure binderDoc
                         , docAddBaseY BrIndentRegular
                         $ docForceParSpacing
                         $ pure bodyDoc
                         ]
                     ]
        ]
      ++ wherePartMultiLine
    -- conservative approach: everything starts on the left.
    addAlternative
      $ docLines
      $ [ docAddBaseY BrIndentRegular
          $ patPartParWrap
          $ docLines
          $ map docSetBaseY
          $ clauseDocs
          >>= \(guardDocs, bodyDoc, _) ->
                (case guardDocs of
                    [] -> []
                    [g] -> [docSeq [appSep $ docLitS "|", pure g]]
                    (g1 : gr) ->
                      (docSeq [appSep $ docLitS "|", pure g1]
                      : (gr <&> \g ->
                          docSeq [docCommaSep, pure g]
                        )
                      )
                  )
                  ++ [ docCols
                         ColOpPrefix
                         [ appSep $ pure binderDoc
                         , docAddBaseY BrIndentRegular $ pure bodyDoc
                         ]
                     ]
        ]
      ++ wherePartMultiLine

-- | Layout a pattern synonym binding
layoutPatSynBind
  :: LocatedN (IdP GhcPs)
  -> HsPatSynDetails GhcPs
  -> HsPatSynDir GhcPs
  -> LPat GhcPs
  -> ToBriDocM BriDocNumbered
layoutPatSynBind name patSynDetails patDir rpat = do
  let
    patDoc    = docLitS "pattern"
    binderDoc = case patDir of
      ImplicitBidirectional   -> docLitS "="
      ExplicitBidirectional{} -> docLitS "<-" -- TODO: confirm whether this is correct
      Unidirectional          -> docLitS "<-"
    body      = colsWrapPat =<< layoutPat rpat
    whereDoc  = docLitS "where"
  mWhereDocs <- layoutPatSynWhere patDir
  headDoc <- fmap pure $ docSeq
    [ patDoc
    , docSeparator
    , layoutLPatSyn name patSynDetails
    , docSeparator
    , binderDoc
    ]
  runFilteredAlternative $ do
    addAlternative
      $
      -- pattern .. where
      --   ..
      --   ..
        docAddBaseY BrIndentRegular
      $ docSeq
          ([headDoc, docSeparator, body] ++ case mWhereDocs of
            Just ds -> [docSeparator, docPar whereDoc (docLines ds)]
            Nothing -> []
          )
    addAlternative
      $
      -- pattern .. =
      --   ..
      -- pattern .. <-
      --   .. where
      --   ..
      --   ..
        docAddBaseY BrIndentRegular
      $ docPar
          headDoc
          (case mWhereDocs of
            Nothing -> body
            Just ds -> docLines ([docSeq [body, docSeparator, whereDoc]] ++ ds)
          )

-- | Helper method for the left hand side of a pattern synonym
layoutLPatSyn
  :: LocatedN (IdP GhcPs)
  -> HsPatSynDetails GhcPs
  -> ToBriDocM BriDocNumbered
layoutLPatSyn name = \case
  PrefixCon _ vars -> do
    let docName = lrdrNameToTextAnn name
        names   = map lrdrNameToTextAnn vars
    docSeq . fmap appSep $ docLit docName : fmap docLit names
  InfixCon left right -> do
    let leftDoc  = lrdrNameToTextAnn left
        docName  = lrdrNameToTextAnn name
        rightDoc = lrdrNameToTextAnn right
    docSeq . fmap (appSep . docLit) $ [leftDoc, docName, rightDoc]
  RecCon recArgs -> do
    let docName = lrdrNameToTextAnn name
        args    = map (lrdrNameToTextAnn . foLabel . recordPatSynField) recArgs
    docSeq . fmap docLit $ [docName, " { "] <> intersperse ", " args <> [" }"]

-- | Helper method to get the where clause from of explicitly bidirectional
-- pattern synonyms
layoutPatSynWhere
  :: HsPatSynDir GhcPs -> ToBriDocM (Maybe [ToBriDocM BriDocNumbered])
layoutPatSynWhere hs = case hs of
  ExplicitBidirectional (MG _ (L _ lbinds)) -> do
    binderDoc <- docLitS "="
    Just
      <$> mapM (docSharedWrapper $ layoutPatternBind Nothing binderDoc) lbinds
  _ -> pure Nothing

--------------------------------------------------------------------------------
-- TyClDecl
--------------------------------------------------------------------------------

layoutTyCl :: LTyClDecl GhcPs -> ToBriDocM BriDocNumbered
layoutTyCl ltycl@(L _loc tycl) = case tycl of
  SynDecl _ name vars fixity typ -> do
    -- hasTrailingParen <- hasAnnKeywordComment ltycl AnnCloseP
    -- let parenWrapper = if hasTrailingParen
    --       then appSep . docWrapNodeAfter ltycl
    --       else id
    let wrapNodeRest :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
        wrapNodeRest = docWrapNodeAfter ltycl
    docWrapNodeBefore ltycl
      $ layoutSynDecl fixity wrapNodeRest name (hsq_explicit vars) typ
  DataDecl _ext name tyVars _ dataDefn ->
    layoutDataDecl ltycl name tyVars dataDefn
  _ -> briDocByExactNoComment ltycl

layoutSynDecl
  :: LexicalFixity
  -> (ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered)
  -> LIdP GhcPs
  -> [LHsTyVarBndr () GhcPs]
  -> LHsType GhcPs
  -> ToBriDocM BriDocNumbered
layoutSynDecl fixity wrapNodeRest name vars typ = do
  let nameStr = lrdrNameToTextAnn name
      lhs = appSep . wrapNodeRest $ case fixity of
        Infix ->
          case vars of
            a : b : rest -> do
              let hasOwnParens = hasAnnKeywordComment a AnnOpenP
              -- This isn't quite right, but does give syntactically valid results
              let needsParens = not (null rest) || hasOwnParens
              docSeq
                $ [docLitS "type", docSeparator]
                ++ [ docParenL | needsParens ]
                ++ [ layoutTyVarBndr False a
                   , docSeparator
                   , docLit nameStr
                   , docSeparator
                   , layoutTyVarBndr False b
                   ]
                ++ [ docParenR | needsParens ]
                ++ fmap (layoutTyVarBndr True) rest
            _ -> error "Not enough for infix case"
        Prefix ->
          docSeq
          $ [ docLitS "type"
            , docSeparator
            , docWrapNodeAround name $ docLit nameStr
            ]
          ++ fmap (layoutTyVarBndr True) vars
  sharedLhs <- docSharedWrapper id lhs
  typeDoc   <- docSharedWrapper layoutType typ
  let hasComments = hasAnyCommentsConnected typ
  layoutLhsAndType hasComments sharedLhs "=" typeDoc

layoutTyVarBndr :: Bool -> LHsTyVarBndr () GhcPs -> ToBriDocM BriDocNumbered
layoutTyVarBndr needsSep lbndr@(L _ bndr) = do
  docWrapNodeBefore lbndr $ case bndr of
    UserTyVar _ _ name -> do
      let nameStr = lrdrNameToTextAnn name
      docSeq $ [ docSeparator | needsSep ] ++ [docLit nameStr]
    KindedTyVar _ _ name kind -> do
      let nameStr = lrdrNameToTextAnn name
      docSeq
        $ [ docSeparator | needsSep ]
        ++ [ docParenL
           , appSep $ docLit nameStr
           , appSep $ docLitS "::"
           , docForceSingleline $ layoutType kind
           , docParenR
           ]

--------------------------------------------------------------------------------
-- TyFamInstDecl
--------------------------------------------------------------------------------

layoutTyFamInstDecl
  :: (Occurrences AnnKeywordId ann, Data ann, Data a)
  => Bool
  -> LocatedAn ann a
  -> TyFamInstDecl GhcPs
  -> ToBriDocM BriDocNumbered
layoutTyFamInstDecl inClass outerNode tfid = do
  let FamEqn _ext name bndrsMay pats _fixity typ = tfid_eqn tfid
      -- bndrsMay isJust e.g. with
      --   type instance forall a . MyType (Maybe a) = Either () a
      innerNode = outerNode
  docWrapNodeBefore outerNode $ do
    let nameStr     = lrdrNameToTextAnn name
        needsParens = hasAnnKeyword outerNode AnnOpenP
        instanceDoc = if inClass
          then docLitS "type"
          else docSeq
            [appSep $ docLitS "type", docLitS "instance"]

        makeForallDoc :: [LHsTyVarBndr () GhcPs] -> ToBriDocM BriDocNumbered
        makeForallDoc bndrs = do
          bndrDocs <- layoutTyVarBndrs bndrs
          docSeq $
            [docLitS "forall"] ++ processTyVarBndrsSingleline bndrDocs

        lhs =
          docWrapNodeAround innerNode . docSeq $ L.concat
            [ [appSep instanceDoc]
            , [makeForallDoc foralls | HsOuterExplicit _ext foralls <- [bndrsMay]]
            , [docParenL | needsParens]
            , [appSep $ docWrapNodeAround name $ docLit nameStr]
            , intersperse docSeparator (layoutHsTyPats pats)
            , [docParenR | needsParens]
            ]
        hasComments =
          hasAnyRegularCommentsConnected outerNode ||
          hasAnyRegularCommentsRest innerNode
    typeDoc <- docSharedWrapper layoutType typ
    layoutLhsAndType hasComments lhs "=" typeDoc

layoutHsTyPats
  :: [HsArg (LHsType GhcPs) (LHsKind GhcPs)] -> [ToBriDocM BriDocNumbered]
layoutHsTyPats pats = pats <&> \case
  HsValArg tm -> layoutType tm
  HsTypeArg _l ty -> docSeq [docLitS "@", layoutType ty]
    -- we ignore the SourceLoc here.. this LPat not being (L _ Pat{}) change
    -- is a bit strange. Hopefully this does not ignore any important
    -- annotations.
  HsArgPar _l -> error "brittany internal error: HsArgPar{}"

--------------------------------------------------------------------------------
-- ClsInstDecl
--------------------------------------------------------------------------------

-- | Layout an @instance@ declaration
--
--   Layout signatures and bindings using the corresponding layouters from the
--   top-level. Layout the instance head, type family instances, and data family
--   instances using ExactPrint.
layoutClsInst :: LClsInstDecl GhcPs -> ToBriDocM BriDocNumbered
layoutClsInst lcid@(L _ cid) = docLines
  [ layoutInstanceHead
  , docEnsureIndent BrIndentRegular
  $ docSetIndentLevel
  $ docSortedLines
  $ fmap layoutAndLocateSig (cid_sigs cid)
  ++ fmap layoutAndLocateBind (bagToList $ cid_binds cid)
  ++ fmap layoutAndLocateTyFamInsts (cid_tyfam_insts cid)
  ++ fmap layoutAndLocateDataFamInsts (cid_datafam_insts cid)
  ]
 where
  layoutInstanceHead :: ToBriDocM BriDocNumbered
  layoutInstanceHead =
    briDocByExactNoComment
      $ InstD NoExtField
      . ClsInstD NoExtField
      . removeChildren
      <$> lcid

  removeChildren :: ClsInstDecl GhcPs -> ClsInstDecl GhcPs
  removeChildren c = c
    { cid_binds = emptyBag
    , cid_sigs = []
    , cid_tyfam_insts = []
    , cid_datafam_insts = []
    }

  -- Like 'docLines', but sorts the lines based on location
  docSortedLines
    :: [ToBriDocM (LocatedAn AnnListItem BriDocNumbered)] -> ToBriDocM BriDocNumbered
  docSortedLines l =
    allocateNode
      . BDFLines
      . fmap unLoc
      . List.sortOn (ExactPrint.rs . locA . getLoc)
      =<< sequence l

  layoutAndLocateSig :: LSig GhcPs -> ToBriDocM (LocatedAn AnnListItem BriDocNumbered)
  layoutAndLocateSig lsig@(L loc _) = L loc <$> layoutSig lsig

  layoutAndLocateBind :: LHsBind GhcPs -> ToBriDocM (LocatedAn AnnListItem BriDocNumbered)
  layoutAndLocateBind lbind@(L loc _) =
    L loc <$> (joinBinds =<< layoutBind lbind)

  joinBinds
    :: Either [BriDocNumbered] BriDocNumbered -> ToBriDocM BriDocNumbered
  joinBinds = \case
    Left ns -> docLines $ pure <$> ns
    Right n -> pure n

  layoutAndLocateTyFamInsts
    :: LTyFamInstDecl GhcPs -> ToBriDocM (LocatedAn AnnListItem BriDocNumbered)
  layoutAndLocateTyFamInsts ltfid@(L loc tfid) =
    L loc <$> layoutTyFamInstDecl True ltfid tfid

  layoutAndLocateDataFamInsts
    :: LDataFamInstDecl GhcPs -> ToBriDocM (LocatedAn AnnListItem BriDocNumbered)
  layoutAndLocateDataFamInsts _ldfid@(L _loc _) =
    -- TODO: must support them natively in brittany - exact print doesn't allow
    -- printing these
    error "Not supported yet"
    -- L loc <$> layoutDataFamInstDecl ldfid

  -- -- Send to ExactPrint then remove unecessary whitespace
  -- layoutDataFamInstDecl :: LDataFamInstDecl GhcPs -> ToBriDocM BriDocNumbered
  -- layoutDataFamInstDecl (L ann x) =
  --   -- ExactPrint adds indentation/newlines to @data@/@type@ declarations
  --   briDocByExactNoComment' stripWhitespace $ GHC.Types.Basic.DataFamInstDeclWithContext
  --     { _dc_a = ann
  --     , _dc_f = GHC.Types.Basic.NotTopLevel
  --     , dc_d = x
  --     }
  --
  -- -- This fixes two issues of output coming from Exactprinting
  -- -- associated (data) type decls. Firstly we place the output into docLines,
  -- -- so one newline coming from Exactprint is superfluous, so we drop the
  -- -- first (empty) line. The second issue is Exactprint indents the first
  -- -- member in a strange fashion:
  -- --
  -- -- input:
  -- --
  -- -- > instance MyClass Int where
  -- -- >   -- | This data is very important
  -- -- >   data MyData = IntData
  -- -- >     { intData  :: String
  -- -- >     , intData2 :: Int
  -- -- >     }
  -- --
  -- -- output of just exactprinting the associated data type syntax node
  -- --
  -- -- >
  -- -- >   -- | This data is very important
  -- -- >   data MyData = IntData
  -- -- >   { intData  :: String
  -- -- >   , intData2 :: Int
  -- -- >   }
  -- --
  -- -- To fix this, we strip whitespace from the start of the comments and the
  -- -- first line of the declaration, stopping when we see "data" or "type" at
  -- -- the start of a line. I.e., this function yields
  -- --
  -- -- > -- | This data is very important
  -- -- > data MyData = IntData
  -- -- >   { intData  :: String
  -- -- >   , intData2 :: Int
  -- -- >   }
  -- --
  -- -- Downside apart from being a hacky and brittle fix is that this removes
  -- -- possible additional indentation from comments before the first member.
  -- --
  -- -- But the whole thing is just a temporary measure until brittany learns
  -- -- to layout data/type decls.
  -- stripWhitespace :: Text -> Text
  -- stripWhitespace t =
  --   Text.intercalate "\n" $ go $ List.drop 1 $ Text.lines t
  --   where
  --     go []               = []
  --     go (line1 : lineR)
  --       | isTypeOrData st = st : lineR
  --       | otherwise       = st : go lineR
  --       where
  --         st = Text.stripStart line1
  --     isTypeOrData t' =
  --       ("type" `Text.isPrefixOf` t')
  --         || ("newtype" `Text.isPrefixOf` t')
  --         || ("data" `Text.isPrefixOf` t')

--------------------------------------------------------------------------------
-- Common Helpers
--------------------------------------------------------------------------------

layoutLhsAndType
  :: Bool
  -> ToBriDocM BriDocNumbered
  -> String
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
layoutLhsAndType hasComments lhs sep typeDoc = do
  runFilteredAlternative $ do
    -- (separators probably are "=" or "::")
    -- lhs = type
    -- lhs :: type
    addAlternativeCond (not hasComments) $
      docSeq
        [lhs, docSeparator, docLitS sep, docSeparator, docForceSingleline typeDoc]
    -- lhs
    --   :: typeA
    --   -> typeB
    -- lhs
    --   =  typeA
    --   -> typeB
    addAlternative
      $ docAddBaseY BrIndentRegular
      $ docPar lhs
      $ docCols
          ColTyOpPrefix
          [ appSep $ docLitS sep
          , docAddBaseY (BrIndentSpecial (length sep + 1)) typeDoc
          ]
