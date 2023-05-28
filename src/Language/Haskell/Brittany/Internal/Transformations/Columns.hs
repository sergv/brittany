{-# LANGUAGE LambdaCase #-}

module Language.Haskell.Brittany.Internal.Transformations.Columns (transformSimplifyColumns) where

import Prelude hiding (lines)

import Data.List qualified as L

import Language.Haskell.Brittany.Internal.RecursionSchemes
import Language.Haskell.Brittany.Internal.Types

transformSimplifyColumns :: BriDoc -> BriDoc
transformSimplifyColumns = cataRewrite alg
  where
    alg :: BriDocF BriDoc -> Maybe BriDoc
    alg = \case
      -- BDWrapAnnKey annKey bd ->
      --   BDWrapAnnKey annKey $ transformSimplify bd
      BDEmpty -> Nothing
      BDLit{} -> Nothing
      BDSeq list
        | any
          (\case
            Fix BDSeq{}   -> True
            Fix BDEmpty{} -> True
            _              -> False
          )
          list
        -> Just $ Fix $ BDSeq $ flip foldMap list $ \case
          Fix BDEmpty   -> []
          Fix (BDSeq l) -> l
          x              -> [x]
      BDSeq (Fix (BDCols sig1 cols1@(_ : _)) : rest)
        | all
          (\case
            Fix BDSeparator -> True
            _                -> False
          )
          rest
        -> Just $ Fix $ BDCols sig1 (L.init cols1 ++ [Fix (BDSeq (L.last cols1 : rest))])
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
      -- prior floating in
      BDAnnotationBefore finalDelta comments (Fix (BDSeq (l : lr))) ->
        Just $ Fix $ BDSeq $ Fix (BDAnnotationBefore finalDelta comments l) : lr
      BDAnnotationBefore finalDelta comments (Fix (BDLines (l : lr))) ->
        Just $ Fix $ BDLines $ Fix (BDAnnotationBefore finalDelta comments l) : lr
      BDAnnotationBefore finalDelta comments (Fix (BDCols sig (l : lr))) ->
        Just $ Fix $ BDCols sig $ Fix (BDAnnotationBefore finalDelta comments l) : lr
      -- post floating in
      BDAnnotationAfter comments (Fix (BDSeq list)) ->
        Just $ Fix $ BDSeq $ L.init list ++ [Fix (BDAnnotationAfter comments (L.last list))]
      BDAnnotationAfter comments (Fix (BDLines list)) ->
        Just $ Fix $ BDLines $ L.init list ++ [Fix (BDAnnotationAfter comments (L.last list))]
      BDAnnotationAfter comments (Fix (BDCols sig cols)) ->
        Just $ Fix $ BDCols sig $ L.init cols ++ [Fix (BDAnnotationAfter comments (L.last cols))]
      BDAnnotationKW kw (Fix (BDSeq list)) ->
        Just $ Fix $ BDSeq $ L.init list ++ [Fix (BDAnnotationKW kw (L.last list))]
      BDAnnotationKW kw (Fix (BDLines list)) ->
        Just $ Fix $ BDLines $ L.init list ++ [Fix (BDAnnotationKW kw (L.last list))]
      BDAnnotationKW kw (Fix (BDCols sig cols)) ->
        Just $ Fix $ BDCols sig $ L.init cols ++ [Fix (BDAnnotationKW kw (L.last cols))]
      -- ensureIndent float-in
      -- not sure if the following rule is necessary; tests currently are
      -- unaffected.
      -- BDEnsureIndent indent (BDLines lines) ->
      --   Just $ BDLines $ BDEnsureIndent indent <$> lines
      -- matching col special transformation
      BDCols sig1 cols1@(_ : _)
        | Fix (BDLines lines@(_ : _ : _)) <- L.last cols1
        , Fix (BDCols sig2 cols2) <- L.last lines
        , sig1 == sig2
        -> Just $ Fix $ BDLines
          [ Fix $ BDCols sig1 $ L.init cols1 ++ [Fix $ BDLines $ L.init lines]
          , Fix $ BDCols sig2 cols2
          ]
      BDCols sig1 cols1@(_ : _)
        | Fix (BDLines lines@(_ : _ : _))                   <- L.last cols1
        , Fix (BDEnsureIndent _ (Fix (BDCols sig2 cols2))) <- L.last lines
        , sig1 == sig2
        -> Just $ Fix $ BDLines
          [ Fix $ BDCols sig1 $ L.init cols1 ++ [Fix $ BDLines $ L.init lines]
          , Fix $ BDCols sig2 cols2
          ]
      BDPar ind col1@(Fix (BDCols sig1 _)) col2@(Fix (BDCols sig2 _))
        | sig1 == sig2
        -> Just $ Fix $ BDAddBaseY ind $ Fix $ BDLines [col1, col2]
      BDPar ind col1@(Fix (BDCols sig1 _)) (Fix (BDLines (col2@(Fix (BDCols sig2 _)) : rest)))
        | sig1 == sig2
        -> Just $ Fix $ BDPar ind (Fix (BDLines [col1, col2])) (Fix (BDLines rest))
      BDPar ind (Fix (BDLines lines1)) col2@(Fix (BDCols sig2 _))
        | Fix (BDCols sig1 _) <- L.last lines1
        , sig1 == sig2
        -> Just $ Fix $ BDAddBaseY ind $ Fix $ BDLines $ lines1 ++ [col2]
      BDPar ind (Fix (BDLines lines1)) (Fix (BDLines (col2@(Fix (BDCols sig2 _)) : rest)))
        | Fix (BDCols sig1 _) <- L.last lines1
        , sig1 == sig2
        -> Just $ Fix $ BDPar ind (Fix (BDLines $ lines1 ++ [col2])) (Fix (BDLines rest))
      -- BDPar ind1 (Fix (BDCols sig1 cols1)) (Fix (BDPar ind2 line (Fix (BDCols sig2 cols2))))
      --   | sig1 == sig2
      --   -> Just $ Fix $ BDPar ind1 $ Fix BDLines [Fix (BDCols sig1 cols1), Fix (BDCols sig)]
      BDCols sig1 cols
        | Fix (BDPar _ind line (Fix (BDCols sig2 cols2))) <- L.last cols
        , sig1 == sig2
        -> Just $ Fix $ BDLines [Fix (BDCols sig1 (L.init cols ++ [line])), Fix (BDCols sig2 cols2)]
      BDCols sig1 cols
        | Fix (BDPar ind line (Fix (BDLines lines))) <- L.last cols
        , Fix (BDCols sig2 cols2)                    <- L.last lines
        , sig1 == sig2
        -> Just $ Fix $ BDLines
          [ Fix $ BDCols sig1 $ L.init cols ++ [Fix $ BDPar ind line $ Fix $ BDLines $ L.init lines]
          , Fix $ BDCols sig2 cols2
          ]
      BDLines [x]            -> Just x
      BDLines []             -> Just $ Fix BDEmpty
      BDSeq{}                -> Nothing
      BDCols{}               -> Nothing
      BDSeparator            -> Nothing
      BDAddBaseY{}           -> Nothing
      BDBaseYPushCur{}       -> Nothing
      BDBaseYPop{}           -> Nothing
      BDIndentLevelPushCur{} -> Nothing
      BDIndentLevelPop{}     -> Nothing
      BDPar{}                -> Nothing
      BDAlt{}                -> Nothing
      BDForceMultiline{}     -> Nothing
      BDForceSingleline{}    -> Nothing
      BDForwardLineMode{}    -> Nothing
      BDExternal{}           -> Nothing
      BDPlain{}              -> Nothing
      BDLines{}              -> Nothing
      BDAnnotationBefore{}   -> Nothing
      BDAnnotationKW{}       -> Nothing
      BDAnnotationAfter{}    -> Nothing
      BDMoveToKWDP{}         -> Nothing
      BDEnsureIndent{}       -> Nothing
      BDSetParSpacing{}      -> Nothing
      BDForceParSpacing{}    -> Nothing
      BDDebug{}              -> Nothing
      BDNonBottomSpacing _ x -> Just x
