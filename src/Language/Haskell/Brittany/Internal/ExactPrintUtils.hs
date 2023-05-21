{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.ExactPrintUtils
  ( withTransformedAnns
  -- , ToplevelAnns(..)
  -- , extractToplevelAnns
  , realSrcSpan'
  ) where

import Control.Monad.Trans.MultiRWS.Strict qualified as MultiRWSS
import Language.Haskell.Brittany.Internal.Config.Types
import GHC.Types.SrcLoc

-- newtype ToplevelAnns = ToplevelAnns { unToplevelAnns :: Map AnnKey Anns }
--
-- -- | Split a set of annotations in a module into a map from top-level module
-- -- elements to the relevant annotations.
-- extractToplevelAnns
--   :: LocatedA HsModule
--   -> Anns
--   -> ToplevelAnns
-- extractToplevelAnns lmod anns = undefined
--   -- _
--   -- where
--   --   ldecls :: _
--   --   L _ HsModule{hsmodDecls} = lmod
--
--   -- ToplevelAnns output
--   -- where
--   --   ldecls :: _
--   --   L _ (HsModule _ _ _ _ ldecls _ _) = lmod
--   --
--   --   declMap1 :: Map ExactPrint.AnnKey ExactPrint.AnnKey
--   --   declMap1 = Map.unions $ ldecls <&> \ldecl ->
--   --     Map.fromSet (const (ExactPrint.mkAnnKey ldecl)) (foldedAnnKeys ldecl)
--   --   declMap2 :: Map ExactPrint.AnnKey ExactPrint.AnnKey
--   --   declMap2 =
--   --     Map.fromList
--   --       $ [ (captured, declMap1 Map.! k)
--   --         | (k, ExactPrint.Ann _ _ _ _ _ (Just captured)) <- Map.toList anns
--   --         ]
--   --   declMap = declMap1 `Map.union` declMap2
--   --   modKey = ExactPrint.mkAnnKey lmod
--   --   output = groupMap (\k _ -> Map.findWithDefault modKey k declMap) anns
--
-- -- groupMap :: (Ord k, Ord l) => (k -> a -> l) -> Map k a -> Map l (Map k a)
-- -- groupMap f = Map.foldlWithKey'
-- --   (\m k a -> Map.alter (insert k a) (f k a) m)
-- --   Map.empty
-- --  where
-- --   insert k a Nothing = Just (Map.singleton k a)
-- --   insert k a (Just m) = Just (Map.insert k a m)

withTransformedAnns
  :: ast
  -> MultiRWSS.MultiRWS '[Config] w s a
  -> MultiRWSS.MultiRWS '[Config] w s a
withTransformedAnns _ast m = m
-- withTransformedAnns ast m = MultiRWSS.mGetRawR >>= \case
--   readers@(conf :+: anns :+: HNil) -> do
--     -- TODO: implement `local` for MultiReader/MultiRWS
--     MultiRWSS.mPutRawR (conf :+: f anns :+: HNil)
--     x <- m
--     MultiRWSS.mPutRawR readers
--     pure x
--  where
--   f :: Anns -> Anns
--   f anns =
--     let
--       ((), (annsBalanced, _), _) =
--         ExactPrint.runTransform anns (commentAnnFixTransformGlob ast)
--     in annsBalanced

realSrcSpan' :: SrcSpan -> RealSrcSpan
realSrcSpan' = \case
  RealSrcSpan x _   -> x
  UnhelpfulSpan err -> error $ "Unhelpful span: " ++ show err

