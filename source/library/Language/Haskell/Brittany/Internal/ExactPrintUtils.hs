{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.ExactPrintUtils
  ( withTransformedAnns
  -- , ToplevelAnns(..)
  -- , extractToplevelAnns
  , realSrcSpan'
  ) where

import qualified Control.Monad.State.Class as State.Class
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import Data.Data
import Data.Foldable
import qualified Data.Generics as SYB
import Data.HList.HList
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import GHC (GenLocated(L))
import qualified GHC hiding (parseModule)
import GHC.Hs
import qualified GHC.Types.SrcLoc as GHC
import GHC.Types.SrcLoc (Located, SrcSpan)
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
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
  :: Data ast
  => ast
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

