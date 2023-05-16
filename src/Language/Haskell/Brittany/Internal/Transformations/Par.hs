{-# LANGUAGE LambdaCase #-}

module Language.Haskell.Brittany.Internal.Transformations.Par (transformSimplifyPar) where

import Prelude hiding (lines)

import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.RecursionSchemes

transformSimplifyPar :: BriDoc -> BriDoc
transformSimplifyPar = cata alg
  where
    alg :: BriDocF BriDoc -> BriDoc
    alg = \case
      -- BDPar BrIndentNone line1 line2 -> Just $ BDLines [line1, line2]
      -- BDPar line indented ->
      --   Just $ BDLines [line, indented]
      -- BDPar ind1 (BDPar ind2 line p1) p2 | ind1==ind2 ->
      --   Just $ BDPar ind1 line (BDLines [p1, p2])
      x@(BDPar _ (Fix (BDPar _ (Fix BDPar{}) _)) _) -> Fix x
      BDPar ind1 (Fix (BDPar ind2 line p1)) (Fix (BDLines indenteds)) ->
        Fix $ BDPar ind1 line $ Fix $ BDLines $ Fix (BDEnsureIndent ind2 p1) : indenteds
      BDPar ind1 (Fix (BDPar ind2 line p1)) p2 ->
        Fix $ BDPar ind1 line $ Fix $ BDLines [Fix (BDEnsureIndent ind2 p1), p2]
      BDLines lines
        | any
          (\case
            Fix BDLines{} -> True
            Fix BDEmpty{} -> True
            _              -> False
          )
          lines
        -> case go lines of
          []  -> Fix BDEmpty
          [x] -> x
          xs  -> Fix $ BDLines xs
        where
          go :: [Fix BriDocF] -> [Fix BriDocF]
          go = concatMap $ \case
            Fix (BDLines l) -> go l
            Fix BDEmpty     -> []
            x                -> [x]
      BDLines [] -> Fix BDEmpty
      BDLines [x] -> x
      -- BDCols sig cols | BDPar ind line indented <- List.last cols ->
      --   Just $ BDPar ind (BDCols sig (List.init cols ++ [line])) indented
      -- BDPar BrIndentNone line indented ->
      --   Just $ BDLines [line, indented]
      BDEnsureIndent BrIndentNone x -> x
      x -> Fix x
