{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr
  ( layoutExpr
  , litBriDoc
  , overLitValBriDoc
  ) where

import Data.Functor
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Traversable
import GHC (GenLocated(L))
import qualified GHC.Data.FastString as FastString
import GHC.Hs
import qualified GHC.OldList as List
import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.Decl
import Language.Haskell.Brittany.Internal.Layouters.Pattern
import Language.Haskell.Brittany.Internal.Layouters.Stmt
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils

layoutExpr :: LHsExpr GhcPs -> ToBriDocM BriDocNumbered
layoutExpr lexpr@(L _ expr) = do
  indentPolicy <- mAsk <&> (_conf_layout >>> _lconfig_indentPolicy >>> confUnpack)
  let allowFreeIndent = indentPolicy == IndentPolicyFree
  docWrapNodeAround lexpr $ case expr of
    HsVar _ vname ->
      docLit $ lrdrNameToTextAnn vname
    HsUnboundVar _ name -> docLitS $ occNameString $ rdrNameOcc name
    HsRecSel{} -> do
      -- TODO
      briDocByExactInlineOnly "HsRecFld" lexpr
    HsOverLabel _ext _ name ->
      let label = FastString.unpackFS name in docLitS $ '#' : label
    HsIPVar _ext (HsIPName name) ->
      let label = FastString.unpackFS name in docLitS $ '?' : label
    HsOverLit _ olit -> do
      allocateNode $ overLitValBriDoc $ ol_val olit
    HsLit _ lit -> do
      allocateNode $ litBriDoc lit
    HsLam _ (MG _ (L _ [lmatch@(L _ match)]))
      | pats                    <- m_pats match
      , GRHSs _ [lgrhs] llocals <- m_grhss match
      , EmptyLocalBinds{}       <- llocals
      , L _ (GRHS _ [] body)    <- lgrhs
      -> do
        patDocs <- for (zip (True : repeat False) pats) $ \(isFirst, p) ->
          fmap pure $ do
            -- this code could be as simple as `colsWrapPat =<< layoutPat p`
            -- if it was not for the following two cases:
            -- \ !x -> x
            -- \ ~x -> x
            -- These make it necessary to special-case an additional separator.
            -- (TODO: we create a BDCols here, but then make it ineffective
            -- by wrapping it in docSeq below. We _could_ add alignments for
            -- stuff like lists-of-lambdas. Nothing terribly important..)
            let shouldPrefixSeparator = case p of
                  L _ LazyPat{} -> isFirst
                  L _ BangPat{} -> isFirst
                  _             -> False
            patDocSeq <- layoutPat p
            fixed <- case Seq.viewl patDocSeq of
              p1 Seq.:< pr | shouldPrefixSeparator -> do
                p1' <- docSeq [docSeparator, pure p1]
                pure (p1' Seq.<| pr)
              _ -> pure patDocSeq
            colsWrapPat fixed
        bodyDoc <-
          docAddBaseY BrIndentRegular <$> docSharedWrapper layoutExpr body
        let
          funcPatternPartLine = docCols
            ColCasePattern
            (patDocs <&> (\p -> docSeq [docForceSingleline p, docSeparator]))
        docAlt
          [ -- single line
            docSeq
            [ docLitS "\\"
            , docWrapNodeAround lmatch $ docForceSingleline funcPatternPartLine
            , appSep $ docLitS "->"
            , docForceSingleline bodyDoc
            ]
            -- double line
          , docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
            (docSeq
              [ docLitS "\\"
              , docWrapNodeAround lmatch $ appSep $ docForceSingleline
                funcPatternPartLine
              , docLitS "->"
              ]
            )
            (docForceSingleline bodyDoc)
            -- wrapped par spacing
          , docSetParSpacing $ docSeq
            [ docLitS "\\"
            , docWrapNodeAround lmatch $ docForceSingleline funcPatternPartLine
            , appSep $ docLitS "->"
            , docForceParSpacing bodyDoc
            ]
            -- conservative
          , docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
            (docSeq
              [ docLitS "\\"
              , docWrapNodeAround lmatch $ appSep $ docForceSingleline
                funcPatternPartLine
              , docLitS "->"
              ]
            )
            (docNonBottomSpacing bodyDoc)
          ]
    HsLam{} -> unknownNodeError "HsLam too complex" lexpr
    HsLamCase _ LamCase (MG _ (L _ [])) -> do
      docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ docLitS "\\case {}"
    HsLamCase _ LamCase (MG _ lmatches@(L _ matches)) -> do
      binderDoc <- docLitS "->"
      funcPatDocs <-
        docWrapNodeAround lmatches
        $ layoutPatternBind Nothing binderDoc
        `mapM` matches
      docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
        (docLitS "\\case")
        (docSetBaseAndIndent
        $ docNonBottomSpacing
        $ docLines
        $ return
        <$> funcPatDocs
        )
    HsLamCase _ LamCases _ -> do
      -- TODO
      briDocByExactInlineOnly "\\cases" lexpr
    HsApp _ exp1@(L _ HsApp{}) exp2 -> do
      let
        gather
          :: [LHsExpr GhcPs]
          -> LHsExpr GhcPs
          -> (LHsExpr GhcPs, [LHsExpr GhcPs])
        gather list = \case
          L _ (HsApp _ l r) -> gather (r : list) l
          x -> (x, list)
      let (headE, paramEs) = gather [exp2] exp1
      let
        colsOrSequence = case headE of
          L _ (HsVar _ (L _ (Unqual occname))) ->
            docCols (ColApp $ Text.pack $ occNameString occname)
          _                                    -> docSeq
      headDoc   <- docSharedWrapper layoutExpr headE
      paramDocs <- docSharedWrapper layoutExpr `mapM` paramEs
      let hasComments = hasAnyCommentsConnected exp2
      runFilteredAlternative $ do
        -- foo x y
        addAlternativeCond (not hasComments)
          $ colsOrSequence
          $ appSep (docForceSingleline headDoc)
          : spacifyDocs (docForceSingleline <$> paramDocs)
        -- foo x
        --     y
        addAlternativeCond allowFreeIndent $ docSeq
          [ appSep (docForceSingleline headDoc)
          , docSetBaseY
          $ docAddBaseY BrIndentRegular
          $ docLines
          $ docForceSingleline
          <$> paramDocs
          ]
        -- foo
        --   x
        --   y
        addAlternative $ docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docForceSingleline headDoc)
          (docNonBottomSpacing $ docLines paramDocs)
        -- ( multi
        --   line
        --   function
        -- )
        --   x
        --   y
        addAlternative $ docAddBaseY BrIndentRegular $ docPar
          headDoc
          (docNonBottomSpacing $ docLines paramDocs)
    HsApp _ exp1 exp2 -> do
      -- TODO: if expDoc1 is some literal, we may want to create a docCols here.
      expDoc1 <- docSharedWrapper layoutExpr exp1
      expDoc2 <- docSharedWrapper layoutExpr exp2
      docAlt
        [ -- func arg
          docSeq
          [appSep $ docForceSingleline expDoc1, docForceSingleline expDoc2]
        , -- func argline1
          --   arglines
          -- e.g.
          -- func Abc
          --   { member1 = True
          --   , member2 = 13
          --   }
          docSetParSpacing -- this is most likely superfluous because
                           -- this is a sequence of a one-line and a par-space
                           -- anyways, so it is _always_ par-spaced.
        $ docAddBaseY BrIndentRegular
        $ docSeq
            [appSep $ docForceSingleline expDoc1, docForceParSpacing expDoc2]
        , -- func
          --   arg
          docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docForceSingleline expDoc1)
          (docNonBottomSpacing expDoc2)
        , -- fu
          --   nc
          --   ar
          --     gument
          docAddBaseY BrIndentRegular $ docPar expDoc1 expDoc2
        ]
    HsAppType _ exp1 _atTok (HsWC _ ty1) -> do
      t <- docSharedWrapper layoutType ty1
      e <- docSharedWrapper layoutExpr exp1
      docAlt
        [ docSeq
          [ docForceSingleline e
          , docSeparator
          , docLitS "@"
          , docForceSingleline t
          ]
        , docPar e (docSeq [docLitS "@", t])
        ]
    OpApp _ expLeft@(L _ OpApp{}) expOp expRight -> do
      let
        gather
          :: [(LHsExpr GhcPs, LHsExpr GhcPs)]
          -> LHsExpr GhcPs
          -> (LHsExpr GhcPs, [(LHsExpr GhcPs, LHsExpr GhcPs)])
        gather opExprList = \case
          (L _ (OpApp _ l1 op1 r1)) -> gather ((op1, r1) : opExprList) l1
          final -> (final, opExprList)
        (leftOperand, appList) = gather [] expLeft
      leftOperandDoc <- docSharedWrapper layoutExpr leftOperand
      appListDocs <- appList `forM` \(x, y) ->
        [ (xD, yD)
        | xD <- docSharedWrapper layoutExpr x
        , yD <- docSharedWrapper layoutExpr y
        ]
      opLastDoc <- docSharedWrapper layoutExpr expOp
      expLastDoc <- docSharedWrapper layoutExpr expRight
      allowSinglelinePar <- do
        let hasComLeft = hasAnyCommentsConnected expLeft
            hasComOp   = hasAnyCommentsConnected expOp
        pure $ not hasComLeft && not hasComOp
      let
        allowPar = case (expOp, expRight) of
          (L _ (HsVar _ (L _ (Unqual occname))), _)
            | occNameString occname == "$" -> True
          (_, L _ (HsApp _ _ (L _ HsVar{}))) -> False
          _ -> True
      runFilteredAlternative $ do
        -- > one + two + three
        -- or
        -- > one + two + case x of
        -- >   _ -> three
        addAlternativeCond allowSinglelinePar $ docSeq
          [ appSep $ docForceSingleline leftOperandDoc
          , docSeq $ appListDocs <&> \(od, ed) -> docSeq
            [appSep $ docForceSingleline od, appSep $ docForceSingleline ed]
          , appSep $ docForceSingleline opLastDoc
          , (if allowPar then docForceParSpacing else docForceSingleline)
            expLastDoc
          ]
        -- this case rather leads to some unfortunate layouting than to anything
        -- useful; disabling for now. (it interfers with cols stuff.)
        -- addAlternative
        --   $ docSetBaseY
        --   $ docPar
        --     leftOperandDoc
        --     ( docLines
        --      $ (appListDocs <&> \(od, ed) -> docCols ColOpPrefix [appSep od, docSetBaseY ed])
        --       ++ [docCols ColOpPrefix [appSep opLastDoc, docSetBaseY expLastDoc]]
        --     )
        -- > one
        -- >   + two
        -- >   + three
        addAlternative $ docPar
          leftOperandDoc
          (docLines
          $ (appListDocs <&> \(od, ed) ->
              docCols ColOpPrefix [appSep od, docSetBaseY ed]
            )
          ++ [docCols ColOpPrefix [appSep opLastDoc, docSetBaseY expLastDoc]]
          )
    OpApp _ expLeft expOp expRight -> do
      expDocLeft <- docSharedWrapper layoutExpr expLeft
      expDocOp <- docSharedWrapper layoutExpr expOp
      expDocRight <- docSharedWrapper layoutExpr expRight
      let
        allowPar = case (expOp, expRight) of
          (L _ (HsVar _ (L _ (Unqual occname))), _)
            | occNameString occname == "$" -> True
          (_, L _ (HsApp _ _ (L _ HsVar{}))) -> False
          _ -> True
      let
        leftIsDoBlock = case expLeft of
          L _ HsDo{} -> True
          _ -> False
      runFilteredAlternative $ do
        -- one-line
        addAlternative $ docSeq
          [ appSep $ docForceSingleline expDocLeft
          , appSep $ docForceSingleline expDocOp
          , docForceSingleline expDocRight
          ]
        -- -- line + freely indented block for right expression
        -- addAlternative
        --   $ docSeq
        --   [ appSep $ docForceSingleline expDocLeft
        --   , appSep $ docForceSingleline expDocOp
        --   , docSetBaseY $ docAddBaseY BrIndentRegular expDocRight
        --   ]
        -- two-line
        addAlternative $ do
          let
            expDocOpAndRight = docForceSingleline $ docCols
              ColOpPrefix
              [appSep $ expDocOp, docSetBaseY expDocRight]
          if leftIsDoBlock
            then docLines [expDocLeft, expDocOpAndRight]
            else docAddBaseY BrIndentRegular
              $ docPar expDocLeft expDocOpAndRight
              -- TODO: in both cases, we don't force expDocLeft to be
              -- single-line, which has certain.. interesting consequences.
              -- At least, the "two-line" label is not entirely
              -- accurate.
        -- one-line + par
        addAlternativeCond allowPar $ docSeq
          [ appSep $ docForceSingleline expDocLeft
          , appSep $ docForceSingleline expDocOp
          , docForceParSpacing expDocRight
          ]
        -- more lines
        addAlternative $ do
          let
            expDocOpAndRight =
              docCols ColOpPrefix [appSep expDocOp, docSetBaseY expDocRight]
          if leftIsDoBlock
            then docLines [expDocLeft, expDocOpAndRight]
            else docAddBaseY BrIndentRegular
              $ docPar expDocLeft expDocOpAndRight
    NegApp _ op _ -> do
      opDoc <- docSharedWrapper layoutExpr op
      docSeq [docLitS "-", opDoc]
    HsPar _ _lparTok innerExp _rparTok -> do
      innerExpDoc <- docSharedWrapper (docWrapNodeAround lexpr . layoutExpr) innerExp
      docAlt
        [ docSeq
          [ docParenL
          , docForceSingleline innerExpDoc
          , docParenR
          ]
        , docSetBaseY $ docLines
          [ docCols
            ColOpPrefix
            [ docParenL
            , docAddBaseY (BrIndentSpecial 2) innerExpDoc
            ]
          , docParenR
          ]
        ]
    SectionL _ left op -> do -- TODO: add to testsuite
      leftDoc <- docSharedWrapper layoutExpr left
      opDoc <- docSharedWrapper layoutExpr op
      docSeq [leftDoc, docSeparator, opDoc]
    SectionR _ op right -> do -- TODO: add to testsuite
      opDoc <- docSharedWrapper layoutExpr op
      rightDoc <- docSharedWrapper layoutExpr right
      docSeq [opDoc, docSeparator, rightDoc]
    ExplicitTuple _ args boxity -> do
      let argExprs = args <&> \arg -> case arg of
            Present _ e -> (arg, Just e)
            Missing _   -> (arg, Nothing)
      argDocs <- for argExprs $ docSharedWrapper $ \(_arg, exprM) ->
        maybe docEmpty layoutExpr exprM
      let hasComments = hasCommentsBetween lexpr AnnOpenP AnnCloseP
          (openLit, closeLit) = case boxity of
            Boxed   -> (docParenL, docParenR)
            Unboxed -> (docParenHashLSep, docParenHashRSep)
      case splitFirstLast argDocs of
        FirstLastEmpty ->
          docSeq [openLit, docNodeAnnKW lexpr (Just AnnOpenP) closeLit]
        FirstLastSingleton e -> docAlt
          [ docCols
            ColTuple
            [ openLit
            , docNodeAnnKW lexpr (Just AnnOpenP) $ docForceSingleline e
            , closeLit
            ]
          , docSetBaseY $ docLines
            [ docSeq
              [ openLit
              , docNodeAnnKW lexpr (Just AnnOpenP) $ docForceSingleline e
              ]
            , closeLit
            ]
          ]
        FirstLast e1 ems eN -> runFilteredAlternative $ do
          addAlternativeCond (not hasComments)
            $ docCols ColTuple
            $ [docSeq [openLit, docForceSingleline e1]]
            ++ (ems <&> \e -> docSeq [docCommaSep, docForceSingleline e])
            ++ [ docSeq
                   [ docCommaSep
                   , docNodeAnnKW lexpr (Just AnnOpenP) (docForceSingleline eN)
                   , closeLit
                   ]
               ]
          addAlternative
            $ let
                start = docCols ColTuples [appSep openLit, e1]
                linesM = ems <&> \d -> docCols ColTuples [docCommaSep, d]
                lineN = docCols
                  ColTuples
                  [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenP) eN]
                end = closeLit
              in docSetBaseY $ docLines $ [start] ++ linesM ++ [lineN, end]
    HsCase _ cExp (MG _ (L _ [])) -> do
      cExpDoc <- docSharedWrapper layoutExpr cExp
      docAlt
        [ docAddBaseY BrIndentRegular $ docSeq
          [ appSep $ docLitS "case"
          , appSep $ docForceSingleline cExpDoc
          , docLitS "of {}"
          ]
        , docPar
          (docAddBaseY BrIndentRegular
          $ docPar (docLitS "case") cExpDoc
          )
          (docLitS "of {}")
        ]
    HsCase _ cExp (MG _ lmatches@(L _ matches)) -> do
      cExpDoc <- docSharedWrapper layoutExpr cExp
      binderDoc <- docLitS "->"
      funcPatDocs <-
        docWrapNodeAround lmatches
        $ layoutPatternBind Nothing binderDoc
        `mapM` matches
      docAlt
        [ docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docSeq
            [ appSep $ docLitS "case"
            , appSep $ docForceSingleline cExpDoc
            , docLitS "of"
            ]
          )
          (docSetBaseAndIndent
          $ docNonBottomSpacing
          $ docLines
          $ return
          <$> funcPatDocs
          )
        , docPar
          (docAddBaseY BrIndentRegular
          $ docPar (docLitS "case") cExpDoc
          )
          (docAddBaseY BrIndentRegular $ docPar
            (docLitS "of")
            (docSetBaseAndIndent
            $ docNonBottomSpacing
            $ docLines
            $ return
            <$> funcPatDocs
            )
          )
        ]
    HsIf _ ifExpr thenExpr elseExpr -> do
      ifExprDoc   <- docSharedWrapper layoutExpr ifExpr
      thenExprDoc <- docSharedWrapper layoutExpr thenExpr
      elseExprDoc <- docSharedWrapper layoutExpr elseExpr
      let hasComments      = hasAnyCommentsBelow lexpr
          maySpecialIndent = case indentPolicy of
            IndentPolicyLeft     -> BrIndentRegular
            IndentPolicyMultiple -> BrIndentRegular
            IndentPolicyFree     -> BrIndentSpecial 3
      -- TODO: some of the alternatives (especially last and last-but-one)
      -- overlap.
      docSetIndentLevel $ runFilteredAlternative $ do
        -- if _ then _ else _
        addAlternativeCond (not hasComments) $ docSeq
          [ appSep $ docLitS "if"
          , appSep $ docForceSingleline ifExprDoc
          , appSep $ docLitS "then"
          , appSep $ docForceSingleline thenExprDoc
          , appSep $ docLitS "else"
          , docForceSingleline elseExprDoc
          ]
        -- either
        --   if expr
        --   then foo
        --     bar
        --   else foo
        --     bar
        -- or
        --   if expr
        --   then
        --     stuff
        --   else
        --     stuff
        -- note that this has par-spacing
        addAlternative $ docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docSeq
            [ docNodeAnnKW lexpr Nothing $ appSep $ docLitS "if"
            , docNodeAnnKW lexpr (Just AnnIf) $ docForceSingleline ifExprDoc
            ]
          )
          (docLines
            [ docAddBaseY BrIndentRegular
            $ docNodeAnnKW lexpr (Just AnnThen)
            $ docNonBottomSpacing
            $ docAlt
                [ docSeq
                  [ appSep $ docLitS "then"
                  , docForceParSpacing thenExprDoc
                  ]
                , docAddBaseY BrIndentRegular
                  $ docPar (docLitS "then") thenExprDoc
                ]
            , docAddBaseY BrIndentRegular $ docNonBottomSpacing $ docAlt
              [ docSeq
                [ appSep $ docLitS "else"
                , docForceParSpacing elseExprDoc
                ]
              , docAddBaseY BrIndentRegular
                $ docPar (docLitS "else") elseExprDoc
              ]
            ]
          )
        -- either
        --   if multi
        --      line
        --      condition
        --   then foo
        --     bar
        --   else foo
        --     bar
        -- or
        --   if multi
        --      line
        --      condition
        --   then
        --     stuff
        --   else
        --     stuff
        -- note that this does _not_ have par-spacing
        addAlternative $ docAddBaseY BrIndentRegular $ docPar
          (docAddBaseY maySpecialIndent $ docSeq
            [ docNodeAnnKW lexpr Nothing $ appSep $ docLitS "if"
            , docNodeAnnKW lexpr (Just AnnIf) $ ifExprDoc
            ]
          )
          (docLines
            [ docAddBaseY BrIndentRegular
            $ docNodeAnnKW lexpr (Just AnnThen)
            $ docAlt
                [ docSeq
                  [ appSep $ docLitS "then"
                  , docForceParSpacing thenExprDoc
                  ]
                , docAddBaseY BrIndentRegular
                  $ docPar (docLitS "then") thenExprDoc
                ]
            , docAddBaseY BrIndentRegular $ docAlt
              [ docSeq
                [ appSep $ docLitS "else"
                , docForceParSpacing elseExprDoc
                ]
              , docAddBaseY BrIndentRegular
                $ docPar (docLitS "else") elseExprDoc
              ]
            ]
          )
        addAlternative $ docSetBaseY $ docLines
          [ docAddBaseY maySpecialIndent $ docSeq
            [ docNodeAnnKW lexpr Nothing $ appSep $ docLitS "if"
            , docNodeAnnKW lexpr (Just AnnIf) $ ifExprDoc
            ]
          , docNodeAnnKW lexpr (Just AnnThen)
          $ docAddBaseY BrIndentRegular
          $ docPar (docLitS "then") thenExprDoc
          , docAddBaseY BrIndentRegular
            $ docPar (docLitS "else") elseExprDoc
          ]
    HsMultiIf _ cases -> do
      let hasComments = hasAnyCommentsBelow lexpr
      clauseDocs <- cases `forM` layoutGrhs
      binderDoc  <- docLitS "->"
      docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
        (docLitS "if")
        (layoutPatternBindFinal
          Nothing
          binderDoc
          Nothing
          clauseDocs
          Nothing
          hasComments
        )
    HsLet _ _letTok binds _inTok exp1 -> do
      let hasComments = hasAnyCommentsBelow lexpr
      expDoc1   <- docSharedWrapper layoutExpr exp1
      -- We jump through some ugly hoops here to ensure proper sharing.
      mBindDocs <- fmap (fmap pure) <$> layoutLocalBinds binds
      let
        ifIndentFreeElse :: a -> a -> a
        ifIndentFreeElse x y = case indentPolicy of
          IndentPolicyLeft     -> y
          IndentPolicyMultiple -> y
          IndentPolicyFree     -> x
      -- this `docSetBaseAndIndent` might seem out of place (especially the
      -- Indent part; setBase is necessary due to the use of docLines below),
      -- but is here due to ghc-exactprint's DP handling of "let" in
      -- particular.
      -- Just pushing another indentation level is a straightforward approach
      -- to making brittany idempotent, even though the result is non-optimal
      -- if "let" is moved horizontally as part of the transformation, as the
      -- comments before the first let item are moved horizontally with it.
      docSetBaseAndIndent $ case mBindDocs of
        Just [bindDoc] -> runFilteredAlternative $ do
          addAlternativeCond (not hasComments) $ docSeq
            [ appSep $ docLitS "let"
            , docNodeAnnKW lexpr (Just AnnLet) $ appSep $ docForceSingleline
              bindDoc
            , appSep $ docLitS "in"
            , docForceSingleline expDoc1
            ]
          addAlternative $ docLines
            [ docNodeAnnKW lexpr (Just AnnLet) $ docAlt
              [ docSeq
                [ appSep $ docLitS "let"
                , ifIndentFreeElse docSetBaseAndIndent docForceSingleline
                  $ bindDoc
                ]
              , docAddBaseY BrIndentRegular $ docPar
                (docLitS "let")
                (docSetBaseAndIndent bindDoc)
              ]
            , docAlt
              [ docSeq
                [ appSep $ docLitS $ ifIndentFreeElse "in " "in"
                , ifIndentFreeElse
                  docSetBaseAndIndent
                  docForceSingleline
                  expDoc1
                ]
              , docAddBaseY BrIndentRegular
                $ docPar (docLitS "in") (docSetBaseY expDoc1)
              ]
            ]
        Just bindDocs@(_ : _) -> runFilteredAlternative $ do
          --either
          --  let
          --    a = b
          --    c = d
          --  in foo
          --    bar
          --    baz
          --or
          --  let
          --    a = b
          --    c = d
          --  in
          --    fooooooooooooooooooo
          let
            noHangingBinds =
              [ docNonBottomSpacing $ docAddBaseY BrIndentRegular $ docPar
                (docLitS "let")
                (docSetBaseAndIndent $ docLines bindDocs)
              , docSeq
                [ docLitS "in "
                , docAddBaseY BrIndentRegular $ docForceParSpacing expDoc1
                ]
              ]
          addAlternative $ case indentPolicy of
            IndentPolicyLeft -> docLines noHangingBinds
            IndentPolicyMultiple -> docLines noHangingBinds
            IndentPolicyFree -> docLines
              [ docNodeAnnKW lexpr (Just AnnLet) $ docSeq
                [ appSep $ docLitS "let"
                , docSetBaseAndIndent $ docLines bindDocs
                ]
              , docSeq [appSep $ docLitS "in ", docSetBaseY expDoc1]
              ]
          addAlternative $ docLines
            [ docNodeAnnKW lexpr (Just AnnLet)
            $ docAddBaseY BrIndentRegular
            $ docPar
                (docLitS "let")
                (docSetBaseAndIndent $ docLines $ bindDocs)
            , docAddBaseY BrIndentRegular
              $ docPar (docLitS "in") (docSetBaseY $ expDoc1)
            ]
        _ -> docSeq [appSep $ docLitS "let in", expDoc1]
      -- docSeq [appSep $ docLitS "let in", expDoc1]
    HsDo _ stmtCtx (L _ stmts) -> case stmtCtx of
      DoExpr _ -> do
        stmtDocs <- traverse (docSharedWrapper layoutStmt) stmts
        docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docLitS "do")
          (docSetBaseAndIndent $ docNonBottomSpacing $ docLines stmtDocs)
      MDoExpr _ -> do
        stmtDocs <- docSharedWrapper layoutStmt `mapM` stmts
        docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docLitS "mdo")
          (docSetBaseAndIndent $ docNonBottomSpacing $ docLines stmtDocs)
      x
        | _:_:_ <- stmts -- has at least 2 elements
        , case x of
            ListComp  -> True
            MonadComp -> True
            _         -> False
        -> do
          let hasComments = hasAnyCommentsBelow lexpr
          stmtDocs <- docSharedWrapper layoutStmt `mapM` stmts
          runFilteredAlternative $ do
            addAlternativeCond (not hasComments) $ docSeq
              [ docNodeAnnKW lexpr Nothing $ appSep $ docLitS "["
              , docNodeAnnKW lexpr (Just AnnOpenS)
                  $ appSep
                  $ docForceSingleline
                  $ List.last stmtDocs
              , appSep $ docLitS "|"
              , docSeq
                  $ List.intersperse docCommaSep
                  $ docForceSingleline <$> List.init stmtDocs
              , docLitS " ]"
              ]
            addAlternative
              $ let
                  start = docCols
                    ColListComp
                    [ docNodeAnnKW lexpr Nothing $ appSep $ docLitS "["
                    , docSetBaseY
                    $ docNodeAnnKW lexpr (Just AnnOpenS)
                    $ List.last stmtDocs
                    ]
                  stmtDocsInit = List.init stmtDocs
                  s1           = List.head stmtDocsInit
                  sM           = List.tail stmtDocsInit
                  line1        =
                    docCols ColListComp [appSep $ docLitS "|", s1]
                  lineM        = sM <&> \d -> docCols ColListComp [docCommaSep, d]
                in docSetBaseY $ docLines $ [start, line1] ++ lineM ++ [docBracketR]
      _ -> do
        -- TODO
        unknownNodeError "HsDo{} unknown stmtCtx" lexpr
    ExplicitList _ elems@(_ : _) -> do
      let hasComments = hasAnyCommentsBelow lexpr
      elemDocs <- traverse (docSharedWrapper layoutExpr) elems
      case splitFirstLast elemDocs of
        FirstLastEmpty -> docSeq
          [ docLitS "["
          , docNodeAnnKW lexpr (Just AnnOpenS) $ docLitS "]"
          ]
        FirstLastSingleton e -> docAlt
          [ docSeq
            [ docLitS "["
            , docNodeAnnKW lexpr (Just AnnOpenS) $ docForceSingleline e
            , docLitS "]"
            ]
          , docSetBaseY $ docLines
            [ docSeq
              [ docLitS "["
              , docSeparator
              , docSetBaseY $ docNodeAnnKW lexpr (Just AnnOpenS) e
              ]
            , docLitS "]"
            ]
          ]
        FirstLast e1 ems eN -> runFilteredAlternative $ do
          addAlternativeCond (not hasComments)
            $ docSeq
            $ [docLitS "["]
            ++ List.intersperse
                 docCommaSep
                 (docForceSingleline
                 <$> (e1 : ems ++ [docNodeAnnKW lexpr (Just AnnOpenS) eN])
                 )
            ++ [docLitS "]"]
          addAlternative
            $ let
                start = docCols ColList [appSep $ docLitS "[", e1]
                linesM = ems <&> \d -> docCols ColList [docCommaSep, d]
                lineN = docCols
                  ColList
                  [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenS) eN]
              in docSetBaseY $ docLines $ [start] ++ linesM ++ [lineN] ++ [docBracketR]
    ExplicitList _ [] -> docLitS "[]"
    RecordCon _ lname fields -> case fields of
      HsRecFields fs Nothing -> do
        let nameDoc = docWrapNodeAround lname $ docLit $ lrdrNameToText lname
        rFs <- for fs $ \lfield@(L _ (HsFieldBind _ann (L _ fieldOcc) rFExpr pun)) -> do
          let FieldOcc _ lnameF = fieldOcc
          rFExpDoc <- if pun
            then pure Nothing
            else Just <$> docSharedWrapper layoutExpr rFExpr
          pure (lfield, lrdrNameToText lnameF, rFExpDoc)
        recordExpression False indentPolicy lexpr nameDoc rFs
      HsRecFields [] (Just (L _ (RecFieldsDotDot 0))) -> do
        let t = lrdrNameToText lname
        docWrapNodeAround lname $ docLit $ t <> Text.pack " { .. }"
      HsRecFields fs@(_ : _) (Just (L _ (RecFieldsDotDot dotdoti))) | dotdoti == length fs -> do
        let nameDoc = docWrapNodeAround lname $ docLit $ lrdrNameToText lname
        fieldDocs <- for fs $ \fieldl@(L _ (HsFieldBind _ann (L _ fieldOcc) fExpr pun)) -> do
          let FieldOcc _ lnameF = fieldOcc
          fExpDoc <- if pun
            then pure Nothing
            else Just <$> docSharedWrapper layoutExpr fExpr
          pure (fieldl, lrdrNameToText lnameF, fExpDoc)
        recordExpression True indentPolicy lexpr nameDoc fieldDocs
      _ -> unknownNodeError "RecordCon with puns" lexpr
    RecordUpd _ rExpr (Left fields) -> do
      rExprDoc <- docSharedWrapper layoutExpr rExpr
      rFs      <- for fields $ \lfield@(L _ (HsFieldBind _ann (L _ ambName) rFExpr pun)) -> do
        rFExpDoc <- if pun
          then pure Nothing
          else Just <$> docSharedWrapper layoutExpr rFExpr
        pure $ case ambName of
          Unambiguous _ n -> (lfield, lrdrNameToText n, rFExpDoc)
          Ambiguous   _ n -> (lfield, lrdrNameToText n, rFExpDoc)
      recordExpression False indentPolicy lexpr rExprDoc rFs
    RecordUpd _ _ (Right _) ->
      unknownNodeError "RecordUpd with RecUpdProj" lexpr
    ExprWithTySig _ e (HsWC _ext t) -> do
      expDoc <- docSharedWrapper layoutExpr e
      typDoc <- docSharedWrapper layoutSigType t
      docSeq [appSep expDoc, appSep $ docLitS "::", typDoc]
    ArithSeq _ Nothing info -> case info of
      From e1 -> do
        e1Doc <- docSharedWrapper layoutExpr e1
        docSeq
          [ docLitS "["
          , appSep $ docForceSingleline e1Doc
          , docLitS "..]"
          ]
      FromThen e1 e2 -> do
        e1Doc <- docSharedWrapper layoutExpr e1
        e2Doc <- docSharedWrapper layoutExpr e2
        docSeq
          [ docLitS "["
          , docForceSingleline e1Doc
          , docCommaSep
          , appSep $ docForceSingleline e2Doc
          , docLitS "..]"
          ]
      FromTo e1 eN -> do
        e1Doc <- docSharedWrapper layoutExpr e1
        eNDoc <- docSharedWrapper layoutExpr eN
        docSeq
          [ docLitS "["
          , appSep $ docForceSingleline e1Doc
          , appSep $ docLitS ".."
          , docForceSingleline eNDoc
          , docLitS "]"
          ]
      FromThenTo e1 e2 eN -> do
        e1Doc <- docSharedWrapper layoutExpr e1
        e2Doc <- docSharedWrapper layoutExpr e2
        eNDoc <- docSharedWrapper layoutExpr eN
        docSeq
          [ docLitS "["
          , docForceSingleline e1Doc
          , docCommaSep
          , appSep $ docForceSingleline e2Doc
          , appSep $ docLitS ".."
          , docForceSingleline eNDoc
          , docLitS "]"
          ]
    ArithSeq{} -> briDocByExactInlineOnly "ArithSeq" lexpr
    HsTypedBracket{} ->
      -- TODO
      briDocByExactInlineOnly "HsTypedBracket{}" lexpr
    HsUntypedBracket{} ->
      -- TODO
      briDocByExactInlineOnly "HsUntypedBracket{}" lexpr
    HsTypedSplice{} ->
      -- TODO
      briDocByExactInlineOnly "HsTypedSplice{}" lexpr
    HsUntypedSplice _ (HsUntypedSpliceExpr _ e) -> do
      docSeq [docLitS "$", docParenL, layoutExpr e, docParenR]
    HsUntypedSplice _ (HsQuasiQuote _ quoter content) -> do
      allocateNode $ BDFPlain
        (Text.pack
        $ "["
        ++ showOutputable quoter
        ++ "|"
        ++ showOutputable content
        ++ "|]"
        )
    HsProc{} -> do
      -- TODO
      briDocByExactInlineOnly "HsProc{}" lexpr
    HsStatic{} -> do
      -- TODO
      briDocByExactInlineOnly "HsStatic{}" lexpr
    ExplicitSum{} -> do
      -- TODO
      briDocByExactInlineOnly "ExplicitSum{}" lexpr
    HsPragE{} -> do
      -- TODO
      briDocByExactInlineOnly "HsPragE{}" lexpr
    HsGetField{} ->
      -- TODO
      briDocByExactInlineOnly "HsGetField" lexpr
    HsProjection{} ->
      -- TODO
      briDocByExactInlineOnly "HsProjection" lexpr

recordExpression
  :: Bool
  -> IndentPolicy
  -> LocatedAn ann lExpr
  -> ToBriDocM BriDocNumbered
  -> [ ( LocatedAn ann' name
       , Text
       , Maybe (ToBriDocM BriDocNumbered)
       )
     ]
  -> ToBriDocM BriDocNumbered
recordExpression False _ lexpr nameDoc [] = docSeq
  [ docNodeAnnKW lexpr (Just AnnOpenC) $ docSeq [nameDoc, docLitS "{"]
  , docLitS "}"
  ]
recordExpression True _ lexpr nameDoc [] = docSeq -- this case might still be incomplete, and is probably not used
         -- atm anyway.
  [ docNodeAnnKW lexpr (Just AnnOpenC)
    $ docSeq [nameDoc, docLitS "{"]
  , docLitS " .. }"
  ]
recordExpression dotdot indentPolicy lexpr nameDoc rFs@(rF1 : rFr) = do
  let (rF1f, rF1n, rF1e) = rF1
  runFilteredAlternative $ do
    -- container { fieldA = blub, fieldB = blub }
    addAlternative $ docSeq
      [ docNodeAnnKW lexpr Nothing $ appSep $ docForceSingleline nameDoc
      , appSep $ docLitS "{"
      , docSeq $ List.intersperse docCommaSep $ rFs <&> \case
        (lfield, fieldStr, Just fieldDoc) -> docWrapNodeAround lfield $ docSeq
          [ appSep $ docLit fieldStr
          , appSep $ docLitS "="
          , docForceSingleline fieldDoc
          ]
        (lfield, fieldStr, Nothing) -> docWrapNodeAround lfield $ docLit fieldStr
      , if dotdot
        then docSeq [docCommaSep, docLitS "..", docSeparator]
        else docSeparator
      , docLitS "}"
      ]
    -- hanging single-line fields
    -- container { fieldA = blub
    --           , fieldB = blub
    --           }
    addAlternativeCond (indentPolicy == IndentPolicyFree) $ docSeq
      [ docNodeAnnKW lexpr Nothing $ docForceSingleline $ appSep nameDoc
      , docSetBaseY
      $ docLines
      $ let
          line1 = docCols
            ColRec
            [ appSep $ docLitS "{"
            , docWrapNodeBefore rF1f $ appSep $ docLit rF1n
            , case rF1e of
              Just x -> docWrapNodeAfter rF1f $ docSeq
                [appSep $ docLitS "=", docForceSingleline x]
              Nothing -> docEmpty
            ]
          lineR = rFr <&> \(lfield, fText, fDoc) ->
            docWrapNodeAround lfield $ docCols
              ColRec
              [ docCommaSep
              , appSep $ docLit fText
              , case fDoc of
                Just x ->
                  docSeq [appSep $ docLitS "=", docForceSingleline x]
                Nothing -> docEmpty
              ]
          dotdotLine = if dotdot
            then docCols
              ColRec
              [ docNodeAnnKW lexpr (Just AnnOpenC) docCommaSep
              , docNodeAnnKW lexpr (Just AnnDotdot) $ docLitS ".."
              ]
            else docNodeAnnKW lexpr (Just AnnOpenC) docEmpty
          lineN = docLitS "}"
        in [line1] ++ lineR ++ [dotdotLine, lineN]
      ]
    -- non-hanging with expressions placed to the right of the names
    -- container
    -- { fieldA = blub
    -- , fieldB = potentially
    --     multiline
    -- }
    addAlternative $ docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
      (docNodeAnnKW lexpr Nothing nameDoc)
      (docNonBottomSpacing
      $ docLines
      $ let
          line1 = docCols
            ColRec
            [ appSep $ docLitS "{"
            , docWrapNodeBefore rF1f $ appSep $ docLit rF1n
            , docWrapNodeAfter rF1f $ case rF1e of
              Just x -> runFilteredAlternative $ do
                addAlternativeCond (indentPolicy == IndentPolicyFree) $ do
                  docSeq [appSep $ docLitS "=", docSetBaseY x]
                addAlternative $ do
                  docSeq
                    [appSep $ docLitS "=", docForceParSpacing x]
                addAlternative $ do
                  docAddBaseY BrIndentRegular
                    $ docPar (docLitS "=") x
              Nothing -> docEmpty
            ]
          lineR = rFr <&> \(lfield, fText, fDoc) ->
            docWrapNodeAround lfield $ docCols
              ColRec
              [ docCommaSep
              , appSep $ docLit fText
              , case fDoc of
                Just x -> runFilteredAlternative $ do
                  addAlternativeCond (indentPolicy == IndentPolicyFree) $ do
                    docSeq [appSep $ docLitS "=", docSetBaseY x]
                  addAlternative $ do
                    docSeq
                      [appSep $ docLitS "=", docForceParSpacing x]
                  addAlternative $ do
                    docAddBaseY BrIndentRegular
                      $ docPar (docLitS "=") x
                Nothing -> docEmpty
              ]
          dotdotLine = if dotdot
            then docCols
              ColRec
              [ docNodeAnnKW lexpr (Just AnnOpenC) docCommaSep
              , docNodeAnnKW lexpr (Just AnnDotdot) $ docLitS ".."
              ]
            else docNodeAnnKW lexpr (Just AnnOpenC) docEmpty
          lineN = docLitS "}"
        in [line1] ++ lineR ++ [dotdotLine, lineN]
      )

litBriDoc :: HsLit GhcPs -> BriDocFInt
litBriDoc = \case
  HsChar         (SourceText t) _c             -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ ['\'', c, '\'']
  HsCharPrim     (SourceText t) _c             -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ ['\'', c, '\'']
  HsString       (SourceText t) _fastString    -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ FastString.unpackFS fastString
  HsStringPrim   (SourceText t) _byteString    -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ Data.ByteString.Char8.unpack byteString
  HsInt _        (IL (SourceText t) _ _)       -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsIntPrim      (SourceText t) _i             -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsWordPrim     (SourceText t) _i             -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsInt64Prim    (SourceText t) _i             -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsWord64Prim   (SourceText t) _i             -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsInteger      (SourceText t) _i _type       -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsRat _        FL{fl_text = SourceText t} _  -> BDFLit $ Text.pack t
  HsFloatPrim _  FL{fl_text = SourceText t}    -> BDFLit $ Text.pack t
  HsDoublePrim _ FL{fl_text = SourceText t}    -> BDFLit $ Text.pack t
  _                                            -> error "litBriDoc: literal with no SourceText"

overLitValBriDoc :: OverLitVal -> BriDocFInt
overLitValBriDoc = \case
  HsIntegral (IL (SourceText t) _ _)      -> BDFLit $ Text.pack t
  HsFractional FL{fl_text = SourceText t} -> BDFLit $ Text.pack t
  HsIsString (SourceText t) _             -> BDFLit $ Text.pack t
  _                                       -> error "overLitValBriDoc: literal with no SourceText"
