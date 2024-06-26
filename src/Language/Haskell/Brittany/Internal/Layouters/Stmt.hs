{-# LANGUAGE LambdaCase #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt (layoutStmt) where

import Control.Monad.Trans.MultiRWS (MonadMultiReader(..))
import Data.Semigroup

import GHC (GenLocated(L))
import GHC.Hs
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.Decl
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import Language.Haskell.Brittany.Internal.Layouters.Pattern
import Language.Haskell.Brittany.Internal.Types

layoutStmt :: LStmt GhcPs (LHsExpr GhcPs) -> ToBriDocM BriDocNumbered
layoutStmt lstmt@(L _ stmt) = do
  indentPolicy        <- confUnpack . _lconfig_indentPolicy . _conf_layout <$> mAsk
  indentAmount :: Int <- confUnpack . _lconfig_indentAmount . _conf_layout <$> mAsk
  docWrapNodeAround lstmt $ case stmt of
    LastStmt _ body Nothing _ -> do
      layoutExpr body
    BindStmt _ lPat expr -> do
      patDoc <- fmap pure $ colsWrapPat =<< layoutPat lPat
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
              in f $ pure bindDoc
            ]
          , -- let
              --   bind = expr
            docAddBaseY BrIndentRegular $ docPar
            (docLitS "let")
            (docSetBaseAndIndent $ pure bindDoc)
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
              in f $ docLines $ pure <$> bindDocs
            ]
          -- let
          --   aaa = expra
          --   bbb = exprb
          --   ccc = exprc
          addAlternativeCond (not indentFourPlus)
            $ docAddBaseY BrIndentRegular
            $ docPar
                (docLitS "let")
                (docSetBaseAndIndent $ docLines $ pure <$> bindDocs)
    RecStmt _ (L _ stmts) _ _ _ _ _ -> runFilteredAlternative $ do
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
