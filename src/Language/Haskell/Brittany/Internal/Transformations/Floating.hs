{-# LANGUAGE LambdaCase #-}

module Language.Haskell.Brittany.Internal.Transformations.Floating (transformSimplifyFloating) where

import Prelude hiding (lines)

import Data.List qualified as L
import Language.Haskell.Brittany.Internal.RecursionSchemes
import Language.Haskell.Brittany.Internal.Types

-- note that this is not total, and cannot be with that exact signature.
mergeIndents :: BrIndent -> BrIndent -> BrIndent
mergeIndents BrIndentNone x = x
mergeIndents x BrIndentNone = x
mergeIndents (BrIndentSpecial i) (BrIndentSpecial j) =
  BrIndentSpecial (max i j)
mergeIndents _ _ = error "mergeIndents"

transformSimplifyFloating :: BriDoc -> BriDoc
transformSimplifyFloating = stepFull . stepBO
  -- note that semantically, stepFull is completely sufficient.
  -- but the bottom-up switch-to-top-down-on-match transformation has much
  -- better complexity.
  -- UPDATE: by now, stepBO does more than stepFull; for semantic equivalence
  --         the push/pop cases would need to be copied over
  where
    stepBO :: BriDoc -> BriDoc
    stepBO = cata alg
      where
        alg :: BriDocF BriDoc -> BriDoc
        alg = \case
          x@BDAnnotationBefore{}   -> descendPrior $ Fix x
          x@BDAnnotationKW{}       -> descendKW $ Fix x
          x@BDAnnotationAfter{}    -> descendRest $ Fix x
          x@BDAddBaseY{}           -> descendAddB $ Fix x
          x@BDBaseYPushCur{}       -> descendBYPush $ Fix x
          x@BDBaseYPop{}           -> descendBYPop $ Fix x
          x@BDIndentLevelPushCur{} -> descendILPush $ Fix x
          x@BDIndentLevelPop{}     -> descendILPop $ Fix x
          x                         -> Fix x

    stepFull = cataRewrite alg
      where
        alg :: BriDocF BriDoc -> Maybe BriDoc
        alg = \case
          BDAddBaseY BrIndentNone x -> Just x
          -- AddIndent floats into Lines.
          BDAddBaseY indent (Fix (BDLines lines)) ->
            Just $ Fix $ BDLines $ Fix . BDAddBaseY indent <$> lines
          -- AddIndent floats into last column
          BDAddBaseY indent (Fix (BDCols sig cols)) ->
            Just $ Fix $ BDCols sig $ L.init cols ++ [Fix (BDAddBaseY indent (L.last cols))]
          BDAddBaseY ind (Fix (BDSeq list)) ->
            Just $ Fix $ BDSeq $ L.init list ++ [Fix (BDAddBaseY ind (L.last list))]
          -- merge AddIndent and Par
          BDAddBaseY ind1 (Fix (BDPar ind2 line indented)) ->
            Just $ Fix $ BDPar (mergeIndents ind1 ind2) line indented
          BDAddBaseY _ lit@(Fix BDLit{}) -> Just lit
          BDAddBaseY ind (Fix (BDBaseYPushCur x)) ->
            Just $ Fix $ BDBaseYPushCur $ Fix $ BDAddBaseY ind x
          BDAddBaseY ind (Fix (BDBaseYPop x)) -> Just $ Fix $ BDBaseYPop $ Fix $ BDAddBaseY ind x
          -- prior floating in
          BDAnnotationBefore finalDelta comments (Fix (BDPar ind line indented)) ->
            Just $ Fix $ BDPar ind (Fix (BDAnnotationBefore finalDelta comments line)) indented
          BDAnnotationBefore finalDelta comments (Fix (BDSeq (l : lr))) ->
            Just $ Fix $ BDSeq (Fix (BDAnnotationBefore finalDelta comments l) : lr)
          BDAnnotationBefore finalDelta comments (Fix (BDLines (l : lr))) ->
            Just $ Fix $ BDLines (Fix (BDAnnotationBefore finalDelta comments l) : lr)
          BDAnnotationBefore finalDelta comments (Fix (BDCols sig (l : lr))) ->
            Just $ Fix $ BDCols sig (Fix (BDAnnotationBefore finalDelta comments l) : lr)
          -- EnsureIndent float-in
          -- BDEnsureIndent indent (Fix (BDCols sig (col:colr))) ->
          --   Just $ Fix $ BDCols sig (Fix (BDEnsureIndent indent col) : (Fix . BDAddBaseY indent <$> colr))
          -- not sure if the following rule is necessary; tests currently are
          -- unaffected.
          -- BDEnsureIndent indent (Fix (BDLines lines)) ->
          --   Just $ Fix $ BDLines $ Fix . BDEnsureIndent indent <$> lines
          -- post floating in
          BDAnnotationAfter comments (Fix (BDPar ind line indented)) ->
            Just $ Fix $ BDPar ind line $ Fix $ BDAnnotationAfter comments indented
          BDAnnotationAfter comments (Fix (BDSeq list)) ->
            Just $ Fix $ BDSeq $ L.init list ++ [Fix (BDAnnotationAfter comments (L.last list))]
          BDAnnotationAfter comments (Fix (BDLines list)) ->
            Just $ Fix $ BDLines $ L.init list ++ [Fix (BDAnnotationAfter comments (L.last list))]
          BDAnnotationAfter comments (Fix (BDCols sig cols)) ->
            Just $ Fix $ BDCols sig $ L.init cols ++ [Fix (BDAnnotationAfter comments (L.last cols))]
          _ -> Nothing

    descendPrior :: BriDoc -> BriDoc
    descendPrior = anaDescend (coalg . unFix)
      where
        coalg :: BriDocF BriDoc -> Maybe (BriDocF BriDoc)
        coalg = \case
          -- prior floating in
          BDAnnotationBefore finalDelta comments (Fix (BDPar ind line indented)) ->
            Just $ BDPar ind (Fix (BDAnnotationBefore finalDelta comments line)) indented
          BDAnnotationBefore finalDelta comments (Fix (BDSeq (l : lr))) ->
            Just $ BDSeq (Fix (BDAnnotationBefore finalDelta comments l) : lr)
          BDAnnotationBefore finalDelta comments (Fix (BDLines (l : lr))) ->
            Just $ BDLines (Fix (BDAnnotationBefore finalDelta comments l) : lr)
          BDAnnotationBefore finalDelta comments (Fix (BDCols sig (l : lr))) ->
            Just $ BDCols sig (Fix (BDAnnotationBefore finalDelta comments l) : lr)
          BDAnnotationBefore finalDelta comments (Fix (BDAddBaseY indent x)) ->
            Just $ BDAddBaseY indent $ Fix $ BDAnnotationBefore finalDelta comments x
          BDAnnotationBefore finalDelta comments (Fix (BDDebug s x)) ->
            Just $ BDDebug s $ Fix $ BDAnnotationBefore finalDelta comments x
          _ -> Nothing

    descendRest :: BriDoc -> BriDoc
    descendRest = anaDescend (coalg . unFix)
      where
        coalg :: BriDocF BriDoc -> Maybe (BriDocF BriDoc)
        coalg = \case
          -- post floating in
          BDAnnotationAfter comments (Fix (BDPar ind line indented)) ->
            Just $ BDPar ind line $ Fix $ BDAnnotationAfter comments indented
          BDAnnotationAfter comments (Fix (BDSeq list)) ->
            Just $ BDSeq $ L.init list ++ [Fix (BDAnnotationAfter comments (L.last list))]
          BDAnnotationAfter comments (Fix (BDLines list)) ->
            Just $ BDLines $ L.init list ++ [Fix (BDAnnotationAfter comments (L.last list))]
          BDAnnotationAfter comments (Fix (BDCols sig cols)) ->
            Just $ BDCols sig $ L.init cols ++ [Fix (BDAnnotationAfter comments (L.last cols))]
          BDAnnotationAfter comments (Fix (BDAddBaseY indent x)) ->
            Just $ BDAddBaseY indent $ Fix $ BDAnnotationAfter comments x
          BDAnnotationAfter comments (Fix (BDDebug s x)) ->
            Just $ BDDebug s $ Fix $ BDAnnotationAfter comments x
          _ -> Nothing

    descendKW :: BriDoc -> BriDoc
    descendKW = anaDescend (coalg . unFix)
      where
        coalg :: BriDocF BriDoc -> Maybe (BriDocF BriDoc)
        coalg = \case
          -- post floating in
          BDAnnotationKW kw (Fix (BDPar ind line indented)) ->
            Just $ BDPar ind line $ Fix $ BDAnnotationKW kw indented
          BDAnnotationKW kw (Fix (BDSeq list)) ->
            Just $ BDSeq $ L.init list ++ [Fix (BDAnnotationKW kw (L.last list))]
          BDAnnotationKW kw (Fix (BDLines list)) ->
            Just $ BDLines $ L.init list ++ [Fix (BDAnnotationKW kw (L.last list))]
          BDAnnotationKW kw (Fix (BDCols sig cols)) ->
            Just $ BDCols sig $ L.init cols ++ [Fix (BDAnnotationKW kw (L.last cols))]
          BDAnnotationKW kw (Fix (BDAddBaseY indent x)) ->
            Just $ BDAddBaseY indent $ Fix $ BDAnnotationKW kw x
          BDAnnotationKW kw (Fix (BDDebug s x)) ->
            Just $ BDDebug s $ Fix $ BDAnnotationKW kw x
          _ -> Nothing

    descendBYPush :: BriDoc -> BriDoc
    descendBYPush = anaDescend (coalg . unFix)
      where
        coalg :: BriDocF BriDoc -> Maybe (BriDocF BriDoc)
        coalg = \case
          BDBaseYPushCur (Fix (BDCols sig cols@(_ : _))) ->
            Just $ BDCols sig $ Fix (BDBaseYPushCur (L.head cols)) : L.tail cols
          BDBaseYPushCur (Fix (BDDebug s x)) ->
            Just $ BDDebug s $ Fix $ BDBaseYPushCur x
          _ -> Nothing

    descendBYPop :: BriDoc -> BriDoc
    descendBYPop = anaDescend (coalg . unFix)
      where
        coalg :: BriDocF BriDoc -> Maybe (BriDocF BriDoc)
        coalg = \case
          BDBaseYPop (Fix (BDCols sig cols@(_ : _))) ->
            Just $ BDCols sig $ L.init cols ++ [Fix (BDBaseYPop (L.last cols))]
          BDBaseYPop (Fix (BDDebug s x)) ->
            Just $ BDDebug s $ Fix $ BDBaseYPop x
          _ -> Nothing

    descendILPush :: BriDoc -> BriDoc
    descendILPush = anaDescend (coalg . unFix)
      where
        coalg :: BriDocF BriDoc -> Maybe (BriDocF BriDoc)
        coalg = \case
          BDIndentLevelPushCur (Fix (BDCols sig cols@(_ : _))) ->
            Just $ BDCols sig (Fix (BDIndentLevelPushCur (L.head cols)) : L.tail cols)
          BDIndentLevelPushCur (Fix (BDDebug s x)) ->
            Just $ BDDebug s $ Fix $ BDIndentLevelPushCur x
          _ -> Nothing

    descendILPop :: BriDoc -> BriDoc
    descendILPop = anaDescend (coalg . unFix)
      where
        coalg :: BriDocF BriDoc -> Maybe (BriDocF BriDoc)
        coalg = \case
          BDIndentLevelPop (Fix (BDCols sig cols@(_ : _))) ->
            Just $ BDCols sig $ L.init cols ++ [Fix (BDIndentLevelPop (L.last cols))]
          BDIndentLevelPop (Fix (BDDebug s x)) ->
            Just $ BDDebug s $ Fix $ BDIndentLevelPop x
          _ -> Nothing

    descendAddB :: BriDoc -> BriDoc
    descendAddB = anaDescend (coalg . unFix)
      where
        coalg :: BriDocF BriDoc -> Maybe (BriDocF BriDoc)
        coalg = \case
          BDAddBaseY BrIndentNone (Fix x) -> Just x
          -- AddIndent floats into Lines.
          BDAddBaseY indent (Fix (BDLines lines)) ->
            Just $ BDLines $ Fix . BDAddBaseY indent <$> lines
          -- AddIndent floats into last column
          BDAddBaseY indent (Fix (BDCols sig cols)) ->
            Just $ BDCols sig $ L.init cols ++ [Fix (BDAddBaseY indent (L.last cols))]
          -- merge AddIndent and Par
          BDAddBaseY ind1 (Fix (BDPar ind2 line indented)) ->
            Just $ BDPar (mergeIndents ind1 ind2) line indented
          BDAddBaseY ind (Fix (BDAnnotationBefore finalDelta comments x)) ->
            Just $ BDAnnotationBefore finalDelta comments (Fix (BDAddBaseY ind x))
          BDAddBaseY ind (Fix (BDAnnotationAfter comments x)) ->
            Just $ BDAnnotationAfter comments $ Fix $ BDAddBaseY ind x
          BDAddBaseY ind (Fix (BDAnnotationKW kw x)) ->
            Just $ BDAnnotationKW kw $ Fix $ BDAddBaseY ind x
          BDAddBaseY ind (Fix (BDSeq list)) ->
            Just $ BDSeq $ L.init list ++ [Fix (BDAddBaseY ind (L.last list))]
          BDAddBaseY _ (Fix lit@BDLit{}) -> Just lit
          BDAddBaseY ind (Fix (BDBaseYPushCur x)) ->
            Just $ BDBaseYPushCur $ Fix $ BDAddBaseY ind x
          BDAddBaseY ind (Fix (BDBaseYPop x)) ->
            Just $ BDBaseYPop $ Fix $ BDAddBaseY ind x
          BDAddBaseY ind (Fix (BDDebug s x)) ->
            Just $ BDDebug s $ Fix $ BDAddBaseY ind x
          BDAddBaseY ind (Fix (BDIndentLevelPop x)) ->
            Just $ BDIndentLevelPop $ Fix $ BDAddBaseY ind x
          BDAddBaseY ind (Fix (BDIndentLevelPushCur x)) ->
            Just $ BDIndentLevelPushCur $ Fix $ BDAddBaseY ind x
          BDAddBaseY ind (Fix (BDEnsureIndent ind2 x)) ->
            Just $ BDEnsureIndent (mergeIndents ind ind2) x
          _ -> Nothing
