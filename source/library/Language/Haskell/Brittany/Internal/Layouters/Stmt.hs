{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE PackageImports #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt where

import qualified Data.Semigroup as Semigroup
import GHC (GenLocated(L))
import GHC.Hs
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.Decl
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import Language.Haskell.Brittany.Internal.Layouters.Pattern
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types

layoutStmt :: LStmt GhcPs (LHsExpr GhcPs) -> ToBriDocM BriDocNumbered
layoutStmt lstmt@(L _ stmt) = do
  indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
  indentAmount :: Int <-
    mAsk <&> _conf_layout .> _lconfig_indentAmount .> confUnpack
  docWrapNode lstmt $ case stmt of
    LastStmt _ body Nothing _ -> do
      layoutExpr body
    BindStmt _ lPat expr -> do
      patDoc <- fmap return $ colsWrapPat =<< layoutPat lPat
      expDoc <- docSharedWrapper layoutExpr expr
      docAlt
        [ docCols
          ColBindStmt
          [ appSep patDoc
          , docSeq
            [ appSep $ docLitS "<-"
            , docAddBaseY BrIndentRegular $ docForceParSpacing expDoc
            ]
          ]
        , docCols
          ColBindStmt
          [ appSep patDoc
          , docAddBaseY BrIndentRegular
            $ docPar (docLitS "<-") (expDoc)
          ]
        ]
    LetStmt _ binds -> do
      let isFree = indentPolicy == IndentPolicyFree
      let indentFourPlus = indentAmount >= 4
      layoutLocalBinds binds >>= \case
        Nothing -> docLitS "let"
          -- i just tested the above, and it is indeed allowed. heh.
        Just [] -> docLitS "let" -- this probably never happens
        Just [bindDoc] -> docAlt
          [ -- let bind = expr
            docCols
            ColDoLet
            [ appSep $ docLitS "let"
            , let
                f = case indentPolicy of
                  IndentPolicyFree -> docSetBaseAndIndent
                  IndentPolicyLeft -> docForceSingleline
                  IndentPolicyMultiple
                    | indentFourPlus -> docSetBaseAndIndent
                    | otherwise -> docForceSingleline
              in f $ return bindDoc
            ]
          , -- let
              --   bind = expr
            docAddBaseY BrIndentRegular $ docPar
            (docLitS "let")
            (docSetBaseAndIndent $ return bindDoc)
          ]
        Just bindDocs -> runFilteredAlternative $ do
          -- let aaa = expra
          --     bbb = exprb
          --     ccc = exprc
          addAlternativeCond (isFree || indentFourPlus) $ docSeq
            [ appSep $ docLitS "let"
            , let
                f = if indentFourPlus
                  then docEnsureIndent BrIndentRegular
                  else docSetBaseAndIndent
              in f $ docLines $ return <$> bindDocs
            ]
          -- let
          --   aaa = expra
          --   bbb = exprb
          --   ccc = exprc
          addAlternativeCond (not indentFourPlus)
            $ docAddBaseY BrIndentRegular
            $ docPar
                (docLitS "let")
                (docSetBaseAndIndent $ docLines $ return <$> bindDocs)
    RecStmt _ stmts _ _ _ _ _ -> runFilteredAlternative $ do
      -- rec stmt1
      --     stmt2
      --     stmt3
      addAlternativeCond (indentPolicy == IndentPolicyFree) $ docSeq
        [ docLitS "rec"
        , docSeparator
        , docSetBaseAndIndent $ docLines $ layoutStmt <$> stmts
        ]
      -- rec
      --   stmt1
      --   stmt2
      --   stmt3
      addAlternative $ docAddBaseY BrIndentRegular $ docPar
        (docLitS "rec")
        (docLines $ layoutStmt <$> stmts)
    BodyStmt _ expr _ _ -> do
      expDoc <- docSharedWrapper layoutExpr expr
      docAddBaseY BrIndentRegular $ expDoc
    _ -> briDocByExactInlineOnly "some unknown statement" lstmt
