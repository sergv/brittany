{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr
  ( layoutExpr
  , litBriDoc
  , overLitValBriDoc
  ) where

import GHC.Hs
import Language.Haskell.Brittany.Internal.Types


layoutExpr :: LHsExpr GhcPs -> ToBriDocM BriDocNumbered

-- layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))

litBriDoc :: HsLit GhcPs -> BriDocF BriDocNumbered

overLitValBriDoc :: OverLitVal -> BriDocF BriDocNumbered
