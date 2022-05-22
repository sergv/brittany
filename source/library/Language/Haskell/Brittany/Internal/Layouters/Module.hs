{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.Layouters.Module
  ( layoutModule
  -- For testing
  , CommentedImport(..)
  , ImportStatementData(..)
  , transformToCommentedImport
  ) where

import Data.Bifunctor
import Data.List (concatMap, concat)
import qualified Data.Maybe
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import Data.Traversable
import GHC (AnnKeywordId(..), GenLocated(L), moduleNameString, unLoc)
import GHC.Hs
import qualified GHC.OldList as List
import GHC.Parser.Annotation (DeltaPos(..))
import GHC.Unit.Module.Name
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE
import Language.Haskell.Brittany.Internal.Layouters.Import
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.GHC.ExactPrint as ExactPrint
import Language.Haskell.GHC.ExactPrint.Types (commentContents)

layoutModule :: GenLocated ann HsModule -> ToBriDocM BriDocNumbered
layoutModule lmod@(L _ mod') = case mod' of
    -- Implicit module Main
  HsModule{hsmodName = Nothing, hsmodImports} -> do
    let commentedImports = transformToCommentedImport hsmodImports
    docLines (commentedImportsToDoc <$> sortCommentedImports commentedImports)
  HsModule{hsmodName = Just n, hsmodExports, hsmodImports} -> do
    let commentedImports = transformToCommentedImport hsmodImports
    let tn = Text.pack $ moduleNameString $ unLoc n
    allowSingleLineExportList <-
      mAsk <&> _conf_layout .> _lconfig_allowSingleLineExportList .> confUnpack
    -- the config should not prevent single-line layout when there is no
    -- export list
    let allowSingleLine = allowSingleLineExportList || Data.Maybe.isNothing hsmodExports
    docLines
      $ docSeq
          [ runFilteredAlternative $ do
            addAlternativeCond allowSingleLine $ docForceSingleline $ docSeq
              [ appSep $ docLit $ Text.pack "module"
              , appSep $ docLit tn
              , appSep $ case hsmodExports of
                Nothing -> docEmpty
                Just x -> layoutLLIEs True KeepItemsUnsorted x
              , docSeparator
              , docLit $ Text.pack "where"
              ]
            addAlternative $ docLines
              [ docAddBaseY BrIndentRegular $ docPar
                  (docSeq [appSep $ docLit $ Text.pack "module", docLit tn])
                  (docSeq
                    [ case hsmodExports of
                      Nothing -> docEmpty
                      Just x -> layoutLLIEs False KeepItemsUnsorted x
                    , docSeparator
                    , docLit $ Text.pack "where"
                    ]
                  )
              ]
          ]
      : (commentedImportsToDoc <$> sortCommentedImports commentedImports)

data CommentedImport a b
  = EmptyLine
  | IndependentComment a
  | ImportStatement (ImportStatementData a b)
  deriving (Eq)

instance Bifunctor CommentedImport where
  bimap f g = \case
    EmptyLine            -> EmptyLine
    IndependentComment a -> IndependentComment (f a)
    ImportStatement x    -> ImportStatement (bimap f g x)

instance (Show a, Show b) => Show (CommentedImport a b) where
  show = \case
    EmptyLine            -> "EmptyLine"
    IndependentComment _ -> "IndependentComment"
    ImportStatement x    -> show x

data ImportStatementData a b = ImportStatementData
  { isdCommentsBefore :: [a]
  , isdCommentsAfter  :: [a]
  , isdImport         :: b
  } deriving (Eq, Show)

type CommentedImport' = CommentedImport Comment (ImportDecl GhcPs)

instance Bifunctor ImportStatementData where
  bimap f g (ImportStatementData xs ys zs) =
    ImportStatementData (map f xs) (map f ys) (g zs)

_lineDelta :: EpAnn ann -> Maybe Int
_lineDelta x = case anchor_op (entry x) of
  UnchangedAnchor                      -> Nothing
  MovedAnchor SameLine{}               -> Just 0
  MovedAnchor DifferentLine{deltaLine} -> Just deltaLine

transformToCommentedImport
  :: [LImportDecl GhcPs] -> [CommentedImport']
transformToCommentedImport is =
  flip concatMap is $ \(L (a :: SrcSpanAnn' (EpAnn AnnListItem)) ImportDecl{ideclExt, ideclName}) ->
    let comments :: Maybe EpAnnComments
        comments = case ann a of
          EpAnn{comments} -> Just comments
          EpAnnNotUsed    -> Nothing

        _commentsBefore :: [LEpaComment]
        _commentsBefore = case comments of
          Just EpaComments{priorComments} -> priorComments
          Just EpaCommentsBalanced{}      -> error $
            "Unexpected EpaCommentsBalanced that shold never be created by parser:\n" ++ show (locA a)
          Nothing                         -> []

        res :: [CommentedImport']
        res = []
    in
    res

    -- -- let x :: EpAnn EpAnnImportDecl
    -- --     x = ideclExt
    -- --
    -- --     y    :: GenLocated (SrcAnn AnnListItem) ModuleName
    -- --     ann' :: SrcAnn AnnListItem
    -- --     y'   :: ModuleName
    -- --     y@(L ann' y') = ideclName
  -- -- -- _
  --
  -- _ <- case is of
  --   [L (ann :: SrcSpanAnn' (EpAnn AnnListItem)) ImportDecl{ideclExt, ideclName}] -> do
  --     let x :: EpAnn EpAnnImportDecl
  --         x = ideclExt
  --         y    :: GenLocated (SrcAnn AnnListItem) ModuleName
  --         ann' :: SrcAnn AnnListItem
  --         y'   :: ModuleName
  --         y@(L ann' y') = ideclName
  --     undefined
  --   _ -> undefined
  --
  -- undefined $ concat $ concatMap convertComment finalAcc : finalList

sortCommentedImports
  :: forall a.
     [CommentedImport a (ImportDecl GhcPs)]
  -> [CommentedImport a (ImportDecl GhcPs)]
sortCommentedImports =
  concatMap unpackImports . concatMap mergeGroups . map (fmap (sortGroups)) . groupify
  where
    unpackImports
      :: CommentedImport a (ImportDecl GhcPs)
      -> [CommentedImport a (ImportDecl GhcPs)]
    unpackImports = \case
      l@EmptyLine -> [l]
      l@IndependentComment{} -> [l]
      ImportStatement r ->
        map IndependentComment (isdCommentsBefore r) ++ [ImportStatement r]
    mergeGroups
      :: Either (CommentedImport a (ImportDecl GhcPs)) [ImportStatementData a (ImportDecl GhcPs)]
      -> [CommentedImport a (ImportDecl GhcPs)]
    mergeGroups = \case
      Left  x -> [x]
      Right y -> ImportStatement <$> y
    sortGroups
      :: [ImportStatementData a (ImportDecl GhcPs)]
      -> [ImportStatementData a (ImportDecl GhcPs)]
    sortGroups =
      List.sortOn (moduleNameString . unLoc . ideclName . isdImport)
    groupify
      :: [CommentedImport a (ImportDecl GhcPs)]
      -> [Either (CommentedImport a (ImportDecl GhcPs)) [ImportStatementData a (ImportDecl GhcPs)]]
    groupify cs = go [] cs
      where
        go :: [ImportStatementData a (ImportDecl GhcPs)]
           -> [CommentedImport a (ImportDecl GhcPs)]
           -> [Either (CommentedImport a (ImportDecl GhcPs)) [ImportStatementData a (ImportDecl GhcPs)]]
        go [] = \case
          l@EmptyLine : rest            -> Left l : go [] rest
          l@IndependentComment{} : rest -> Left l : go [] rest
          ImportStatement r : rest      -> go [r] rest
          []                            -> []
        go acc = \case
          l@EmptyLine : rest            -> Right (reverse acc) : Left l : go [] rest
          l@IndependentComment{} : rest -> Left l : Right (reverse acc) : go [] rest
          ImportStatement r : rest      -> go (r : acc) rest
          []                            -> [Right (reverse acc)]

commentedImportsToDoc :: CommentedImport' -> ToBriDocM BriDocNumbered
commentedImportsToDoc = \case
  EmptyLine            -> docLitS "" -- docEmpty will not produce emply line here
  IndependentComment c -> commentToDoc c
  ImportStatement r    -> docSeq (layoutImport (isdImport r) : map commentToDoc (isdCommentsAfter r))
  where
    commentToDoc = docLitS . commentContents
