{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt (layoutStmt) where

import GHC.Hs
import Language.Haskell.Brittany.Internal.Types

layoutStmt :: LStmt GhcPs (LHsExpr GhcPs) -> ToBriDocM BriDocNumbered
