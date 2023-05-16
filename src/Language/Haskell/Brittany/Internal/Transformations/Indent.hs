{-# LANGUAGE LambdaCase #-}

module Language.Haskell.Brittany.Internal.Transformations.Indent (transformSimplifyIndent) where

import Prelude hiding (lines)

import Data.Generics.Uniplate.Direct qualified as Uniplate
import GHC.OldList qualified as List
import Language.Haskell.Brittany.Internal.Types

-- prepare layouting by translating BDPar's, replacing them with Indents and
-- floating those in. This gives a more clear picture of what exactly is
-- affected by what amount of indentation.
transformSimplifyIndent :: BriDoc -> BriDoc
transformSimplifyIndent = Uniplate.rewrite $ \case
  BDPar ind (BDLines lines) indented ->
    Just $ BDEnsureIndent ind $ BDLines $ lines ++ [indented]
  BDPar ind (BDCols sig cols) indented ->
    Just $ BDCols sig (List.init cols ++ [BDPar ind (List.last cols) indented])
  BDPar BrIndentNone _ _ -> Nothing
  BDPar ind x indented ->
    Just $ BDPar BrIndentNone (BDAddBaseY ind x) (BDEnsureIndent ind indented)
  -- BDPar ind x indented ->
  --   Just $ BDLines
  --     [ BDAddBaseY ind x
  --     , BDEnsureIndent ind indented
  --     ]
  BDLines lines
    | any
      (\case
        BDLines{} -> True
        BDEmpty{} -> True
        _ -> False
      )
      lines
    -> Just $ BDLines $ filter isNotEmpty $ lines >>= \case
      BDLines l -> l
      x -> [x]
  BDLines [l] -> Just l
  BDAddBaseY i (BDAnnotationBefore ann x) ->
    Just $ BDAnnotationBefore ann (BDAddBaseY i x)
  BDAddBaseY i (BDAnnotationKW kw x) ->
    Just $ BDAnnotationKW kw (BDAddBaseY i x)
  BDAddBaseY i (BDAnnotationAfter ann x) ->
    Just $ BDAnnotationAfter ann (BDAddBaseY i x)
  BDAddBaseY i (BDSeq l) ->
    Just $ BDSeq $ List.init l ++ [BDAddBaseY i $ List.last l]
  BDAddBaseY i (BDCols sig l) ->
    Just $ BDCols sig $ List.init l ++ [BDAddBaseY i $ List.last l]
  BDAddBaseY _ lit@BDLit{} -> Just lit

  _ -> Nothing
