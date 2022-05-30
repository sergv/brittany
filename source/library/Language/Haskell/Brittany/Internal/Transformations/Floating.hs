{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Transformations.Floating where

import qualified Data.Generics.Uniplate.Direct as Uniplate
import qualified GHC.OldList as List
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils



-- note that this is not total, and cannot be with that exact signature.
mergeIndents :: BrIndent -> BrIndent -> BrIndent
mergeIndents BrIndentNone x = x
mergeIndents x BrIndentNone = x
mergeIndents (BrIndentSpecial i) (BrIndentSpecial j) =
  BrIndentSpecial (max i j)
mergeIndents _ _ = error "mergeIndents"


transformSimplifyFloating :: BriDoc -> BriDoc
transformSimplifyFloating = stepBO .> stepFull
  -- note that semantically, stepFull is completely sufficient.
  -- but the bottom-up switch-to-top-down-on-match transformation has much
  -- better complexity.
  -- UPDATE: by now, stepBO does more than stepFull; for semantic equivalence
  --         the push/pop cases would need to be copied over
 where
  descendPrior = transformDownMay $ \case
    -- prior floating in
    BDAnnotationBefore ann (BDPar ind line indented) ->
      Just $ BDPar ind (BDAnnotationBefore ann line) indented
    BDAnnotationBefore ann (BDSeq (l : lr)) ->
      Just $ BDSeq (BDAnnotationBefore ann l : lr)
    BDAnnotationBefore ann (BDLines (l : lr)) ->
      Just $ BDLines (BDAnnotationBefore ann l : lr)
    BDAnnotationBefore ann (BDCols sig (l : lr)) ->
      Just $ BDCols sig (BDAnnotationBefore ann l : lr)
    BDAnnotationBefore ann (BDAddBaseY indent x) ->
      Just $ BDAddBaseY indent $ BDAnnotationBefore ann x
    BDAnnotationBefore ann (BDDebug s x) ->
      Just $ BDDebug s $ BDAnnotationBefore ann x
    _ -> Nothing
  descendRest = transformDownMay $ \case
    -- post floating in
    BDAnnotationAfter (BDPar ind line indented) ->
      Just $ BDPar ind line $ BDAnnotationAfter indented
    BDAnnotationAfter (BDSeq list) ->
      Just
        $ BDSeq
        $ List.init list
        ++ [BDAnnotationAfter $ List.last list]
    BDAnnotationAfter (BDLines list) ->
      Just
        $ BDLines
        $ List.init list
        ++ [BDAnnotationAfter $ List.last list]
    BDAnnotationAfter (BDCols sig cols) ->
      Just
        $ BDCols sig
        $ List.init cols
        ++ [BDAnnotationAfter $ List.last cols]
    BDAnnotationAfter (BDAddBaseY indent x) ->
      Just $ BDAddBaseY indent $ BDAnnotationAfter x
    BDAnnotationAfter (BDDebug s x) ->
      Just $ BDDebug s $ BDAnnotationAfter x
    _ -> Nothing
  descendKW = transformDownMay $ \case
    -- post floating in
    BDAnnotationKW kw (BDPar ind line indented) ->
      Just $ BDPar ind line $ BDAnnotationKW kw indented
    BDAnnotationKW kw (BDSeq list) ->
      Just
        $ BDSeq
        $ List.init list
        ++ [BDAnnotationKW kw $ List.last list]
    BDAnnotationKW kw (BDLines list) ->
      Just
        $ BDLines
        $ List.init list
        ++ [BDAnnotationKW kw $ List.last list]
    BDAnnotationKW kw (BDCols sig cols) ->
      Just
        $ BDCols sig
        $ List.init cols
        ++ [BDAnnotationKW kw $ List.last cols]
    BDAnnotationKW kw (BDAddBaseY indent x) ->
      Just $ BDAddBaseY indent $ BDAnnotationKW kw x
    BDAnnotationKW kw (BDDebug s x) ->
      Just $ BDDebug s $ BDAnnotationKW kw x
    _ -> Nothing
  descendBYPush = transformDownMay $ \case
    BDBaseYPushCur (BDCols sig cols@(_ : _)) ->
      Just $ BDCols sig (BDBaseYPushCur (List.head cols) : List.tail cols)
    BDBaseYPushCur (BDDebug s x) -> Just $ BDDebug s (BDBaseYPushCur x)
    _ -> Nothing
  descendBYPop = transformDownMay $ \case
    BDBaseYPop (BDCols sig cols@(_ : _)) ->
      Just $ BDCols sig (List.init cols ++ [BDBaseYPop (List.last cols)])
    BDBaseYPop (BDDebug s x) -> Just $ BDDebug s (BDBaseYPop x)
    _ -> Nothing
  descendILPush = transformDownMay $ \case
    BDIndentLevelPushCur (BDCols sig cols@(_ : _)) ->
      Just $ BDCols sig (BDIndentLevelPushCur (List.head cols) : List.tail cols)
    BDIndentLevelPushCur (BDDebug s x) ->
      Just $ BDDebug s (BDIndentLevelPushCur x)
    _ -> Nothing
  descendILPop = transformDownMay $ \case
    BDIndentLevelPop (BDCols sig cols@(_ : _)) ->
      Just $ BDCols sig (List.init cols ++ [BDIndentLevelPop (List.last cols)])
    BDIndentLevelPop (BDDebug s x) -> Just $ BDDebug s (BDIndentLevelPop x)
    _ -> Nothing
  descendAddB = transformDownMay $ \case
    BDAddBaseY BrIndentNone x -> Just x
    -- AddIndent floats into Lines.
    BDAddBaseY indent (BDLines lines) ->
      Just $ BDLines $ BDAddBaseY indent <$> lines
    -- AddIndent floats into last column
    BDAddBaseY indent (BDCols sig cols) ->
      Just $ BDCols sig $ List.init cols ++ [BDAddBaseY indent $ List.last cols]
    -- merge AddIndent and Par
    BDAddBaseY ind1 (BDPar ind2 line indented) ->
      Just $ BDPar (mergeIndents ind1 ind2) line indented
    BDAddBaseY ind (BDAnnotationBefore ann x) ->
      Just $ BDAnnotationBefore ann (BDAddBaseY ind x)
    BDAddBaseY ind (BDAnnotationAfter x) ->
      Just $ BDAnnotationAfter (BDAddBaseY ind x)
    BDAddBaseY ind (BDAnnotationKW kw x) ->
      Just $ BDAnnotationKW kw (BDAddBaseY ind x)
    BDAddBaseY ind (BDSeq list) ->
      Just $ BDSeq $ List.init list ++ [BDAddBaseY ind (List.last list)]
    BDAddBaseY _ lit@BDLit{} -> Just $ lit
    BDAddBaseY ind (BDBaseYPushCur x) ->
      Just $ BDBaseYPushCur (BDAddBaseY ind x)
    BDAddBaseY ind (BDBaseYPop x) -> Just $ BDBaseYPop (BDAddBaseY ind x)
    BDAddBaseY ind (BDDebug s x) -> Just $ BDDebug s (BDAddBaseY ind x)
    BDAddBaseY ind (BDIndentLevelPop x) ->
      Just $ BDIndentLevelPop (BDAddBaseY ind x)
    BDAddBaseY ind (BDIndentLevelPushCur x) ->
      Just $ BDIndentLevelPushCur (BDAddBaseY ind x)
    BDAddBaseY ind (BDEnsureIndent ind2 x) ->
      Just $ BDEnsureIndent (mergeIndents ind ind2) x
    _ -> Nothing
  stepBO :: BriDoc -> BriDoc
  stepBO = -- traceFunctionWith "stepBO" (show . briDocToDocWithAnns) (show . briDocToDocWithAnns) $
           transformUp f
   where
    f = \case
      x@BDAnnotationBefore{}   -> descendPrior x
      x@BDAnnotationKW{}       -> descendKW x
      x@BDAnnotationAfter{}    -> descendRest x
      x@BDAddBaseY{}           -> descendAddB x
      x@BDBaseYPushCur{}       -> descendBYPush x
      x@BDBaseYPop{}           -> descendBYPop x
      x@BDIndentLevelPushCur{} -> descendILPush x
      x@BDIndentLevelPop{}     -> descendILPop x
      x                        -> x
  stepFull = -- traceFunctionWith "stepFull" (show . briDocToDocWithAnns) (show . briDocToDocWithAnns) $
             Uniplate.rewrite $ \case
    BDAddBaseY BrIndentNone x -> Just $ x
    -- AddIndent floats into Lines.
    BDAddBaseY indent (BDLines lines) ->
      Just $ BDLines $ BDAddBaseY indent <$> lines
    -- AddIndent floats into last column
    BDAddBaseY indent (BDCols sig cols) ->
      Just $ BDCols sig $ List.init cols ++ [BDAddBaseY indent $ List.last cols]
    BDAddBaseY ind (BDSeq list) ->
      Just $ BDSeq $ List.init list ++ [BDAddBaseY ind (List.last list)]
    -- merge AddIndent and Par
    BDAddBaseY ind1 (BDPar ind2 line indented) ->
      Just $ BDPar (mergeIndents ind1 ind2) line indented
    BDAddBaseY _ lit@BDLit{} -> Just $ lit
    BDAddBaseY ind (BDBaseYPushCur x) ->
      Just $ BDBaseYPushCur (BDAddBaseY ind x)
    BDAddBaseY ind (BDBaseYPop x) -> Just $ BDBaseYPop (BDAddBaseY ind x)
    -- prior floating in
    BDAnnotationBefore ann (BDPar ind line indented) ->
      Just $ BDPar ind (BDAnnotationBefore ann line) indented
    BDAnnotationBefore ann (BDSeq (l : lr)) ->
      Just $ BDSeq ((BDAnnotationBefore ann l) : lr)
    BDAnnotationBefore ann (BDLines (l : lr)) ->
      Just $ BDLines ((BDAnnotationBefore ann l) : lr)
    BDAnnotationBefore ann (BDCols sig (l : lr)) ->
      Just $ BDCols sig ((BDAnnotationBefore ann l) : lr)
    -- EnsureIndent float-in
    -- BDEnsureIndent indent (BDCols sig (col:colr)) ->
    --   Just $ BDCols sig (BDEnsureIndent indent col : (BDAddBaseY indent <$> colr))
    -- not sure if the following rule is necessary; tests currently are
    -- unaffected.
    -- BDEnsureIndent indent (BDLines lines) ->
    --   Just $ BDLines $ BDEnsureIndent indent <$> lines
    -- post floating in
    BDAnnotationAfter (BDPar ind line indented) ->
      Just $ BDPar ind line $ BDAnnotationAfter indented
    BDAnnotationAfter (BDSeq list) ->
      Just
        $ BDSeq
        $ List.init list
        ++ [BDAnnotationAfter $ List.last list]
    BDAnnotationAfter (BDLines list) ->
      Just
        $ BDLines
        $ List.init list
        ++ [BDAnnotationAfter $ List.last list]
    BDAnnotationAfter (BDCols sig cols) ->
      Just
        $ BDCols sig
        $ List.init cols
        ++ [BDAnnotationAfter $ List.last cols]
    _ -> Nothing
