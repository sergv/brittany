{-# LANGUAGE LambdaCase #-}

module Language.Haskell.Brittany.Internal.Transformations.Indent (transformSimplifyIndent) where

import Prelude hiding (lines)

import Data.List qualified as L

import Language.Haskell.Brittany.Internal.RecursionSchemes
import Language.Haskell.Brittany.Internal.Types

-- prepare layouting by translating BDPar's, replacing them with Indents and
-- floating those in. This gives a more clear picture of what exactly is
-- affected by what amount of indentation.
transformSimplifyIndent :: BriDoc -> BriDoc
transformSimplifyIndent = cataRewrite alg
  where
    alg :: BriDocF BriDoc -> Maybe BriDoc
    alg = \case
      BDPar ind (Fix (BDLines lines)) indented ->
        Just $ Fix $ BDEnsureIndent ind $ Fix $ BDLines $ lines ++ [indented]
      BDPar ind (Fix (BDCols sig cols)) indented ->
        Just $ Fix $ BDCols sig $ L.init cols ++ [Fix (BDPar ind (L.last cols) indented)]
      BDPar BrIndentNone _ _ -> Nothing
      BDPar ind x indented ->
        Just $ Fix $ BDPar BrIndentNone (Fix (BDAddBaseY ind x)) (Fix (BDEnsureIndent ind indented))
      -- BDPar ind x indented ->
      --   Just $ BDLines
      --     [ BDAddBaseY ind x
      --     , BDEnsureIndent ind indented
      --     ]
      BDLines lines
        | any
          (\case
            Fix BDLines{} -> True
            Fix BDEmpty{} -> True
            _              -> False
          )
          lines
        -> Just $ Fix $ BDLines $ filter isNotEmpty $ flip foldMap lines $ \case
          Fix (BDLines l) -> l
          x                -> [x]
      BDLines [l] -> Just l
      BDAddBaseY i (Fix (BDAnnotationBefore ann x)) ->
        Just $ Fix $ BDAnnotationBefore ann $ Fix $ BDAddBaseY i x
      BDAddBaseY i (Fix (BDAnnotationKW kw x)) ->
        Just $ Fix $ BDAnnotationKW kw $ Fix $ BDAddBaseY i x
      BDAddBaseY i (Fix (BDAnnotationAfter ann x)) ->
        Just $ Fix $ BDAnnotationAfter ann $ Fix $ BDAddBaseY i x
      BDAddBaseY i (Fix (BDSeq l)) ->
        Just $ Fix $ BDSeq $ L.init l ++ [Fix $ BDAddBaseY i $ L.last l]
      BDAddBaseY i (Fix (BDCols sig l)) ->
        Just $ Fix $ BDCols sig $ L.init l ++ [Fix $ BDAddBaseY i $ L.last l]
      BDAddBaseY _ lit@(Fix BDLit{}) -> Just lit

      _ -> Nothing
