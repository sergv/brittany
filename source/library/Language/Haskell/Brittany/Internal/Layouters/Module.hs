{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.Layouters.Module
  ( layoutModule
  -- For testing
  , CommentedImport(..)
  , ImportStatementData(..)
  , transformToCommentedImport
  ) where

import Data.Bifunctor
import Data.Foldable
import Data.List (concatMap, concat)
import qualified Data.List as L
import Data.List.Reversed (RList)
import qualified Data.List.Reversed as RL
import qualified Data.Maybe
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import Data.Traversable
import GHC (AnnKeywordId(..), GenLocated(L), moduleNameString, unLoc)
import GHC.Generics (Generic)
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
import Language.Haskell.GHC.ExactPrint.Utils
import Prettyprinter.Combinators
import Prettyprinter.Generics

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

data CommentedImport f a b
  = EmptyLine
  | IndependentComment a
  | ImportStatement (ImportStatementData f a b)
  deriving (Eq, Generic)

instance Functor f => Bifunctor (CommentedImport f) where
  bimap f g = \case
    EmptyLine            -> EmptyLine
    IndependentComment a -> IndependentComment (f a)
    ImportStatement x    -> ImportStatement (bimap f g x)

instance (Show (f a), Show a, Show b) => Show (CommentedImport f a b) where
  show = \case
    EmptyLine            -> "EmptyLine"
    IndependentComment _ -> "IndependentComment"
    ImportStatement x    -> show x

data ImportStatementData f a b = ImportStatementData
  { isdCommentsBefore :: f a
  , isdCommentsAfter  :: f a
  , isdImport         :: b
  } deriving (Eq, Show, Generic)

instance (Pretty (f a), Pretty a, Pretty b) => Pretty (CommentedImport f a b) where
  pretty = ppGeneric

instance (Pretty (f a), Pretty b) => Pretty (ImportStatementData f a b) where
  pretty = ppGeneric

type CommentedImport'  = CommentedImport []    Comment (ImportDecl GhcPs)
type CommentedImportRL = CommentedImport RList Comment (ImportDecl GhcPs)

mapCommentedImportContainer :: (forall x. f x -> g x) -> CommentedImport f a b -> CommentedImport g a b
mapCommentedImportContainer f = \case
  EmptyLine            -> EmptyLine
  IndependentComment x -> IndependentComment x
  ImportStatement x    -> ImportStatement (mapImportStatementDataContainer f x)

mapImportStatementDataContainer
  :: (forall x. f x -> g x)
  -> ImportStatementData f a b
  -> ImportStatementData g a b
mapImportStatementDataContainer f ImportStatementData{isdCommentsBefore, isdCommentsAfter, isdImport} =
  ImportStatementData
    { isdCommentsBefore = f isdCommentsBefore
    , isdCommentsAfter  = f isdCommentsAfter
    , isdImport
    }

extractIndependentComment :: CommentedImport f a b -> Maybe a
extractIndependentComment = \case
  EmptyLine            -> Nothing
  IndependentComment x -> Just x
  ImportStatement _    -> Nothing

instance Functor f => Bifunctor (ImportStatementData f) where
  bimap f g (ImportStatementData xs ys zs) =
    ImportStatementData (fmap f xs) (fmap f ys) (g zs)

lineDelta :: DeltaPos -> Int
lineDelta = \case
  SameLine{}               -> 0
  DifferentLine{deltaLine} -> deltaLine

annLineDelta :: EpAnn ann -> Maybe Int
annLineDelta x = case anchor_op (entry x) of
  UnchangedAnchor -> Nothing
  MovedAnchor x   -> Just (lineDelta x)

transformToCommentedImport
  :: [LImportDecl GhcPs] -> [CommentedImport']
transformToCommentedImport =
  map (mapCommentedImportContainer toList) . toList . L.foldl' go RL.Nil
  where
    go :: RList CommentedImportRL -> LImportDecl GhcPs -> RList CommentedImportRL
    go acc (L (a :: SrcSpanAnn' (EpAnn AnnListItem)) importDecl) =
      case annLineDelta (ann a) of
        Nothing ->
          error $ "Unexpected UnchangedAnchor:\n" ++ show (locA a)
        Just n
          | n <= 1    ->
            prefix `RL.snoc`
            ImportStatement ImportStatementData { isdImport = importDecl, isdCommentsBefore = comments, isdCommentsAfter = RL.empty }
          | otherwise ->
            acc' <>
            RL.fromList (replicate (n - 1) EmptyLine) `RL.snoc`
            ImportStatement ImportStatementData { isdImport = importDecl, isdCommentsBefore = RL.empty, isdCommentsAfter = RL.empty }
          where
            (prefix, comments) = RL.spanMaybe extractIndependentComment acc'
      where
        acc' = L.foldl' go' acc commentsBefore

        comments :: Maybe EpAnnComments
        comments = case ann a of
          EpAnn{comments} -> Just comments
          EpAnnNotUsed    -> Nothing

        commentsBefore :: [LEpaComment]
        commentsBefore = case comments of
          Just EpaComments{priorComments} -> priorComments
          Just EpaCommentsBalanced{}      -> error $
            "Unexpected EpaCommentsBalanced that shold never be created by parser:\n" ++ show (locA a)
          Nothing                         -> []

    go' :: RList CommentedImportRL -> LEpaComment -> RList CommentedImportRL
    go' acc comment@(L a' _) =
      case anchor_op a' of
        UnchangedAnchor                      ->
          error $ "Unexpected UnchangedAnchor:\n" ++ show (anchor a')

        MovedAnchor SameLine{}               ->
          case acc of
            RL.Snoc prefix (ImportStatement info@ImportStatementData{isdCommentsAfter}) ->
              prefix `RL.snoc`
              ImportStatement info { isdCommentsAfter = isdCommentsAfter `RL.snoc` tokComment comment }
            other ->
              other `RL.snoc` IndependentComment (tokComment comment)

        MovedAnchor DifferentLine{deltaLine} ->
          acc <>
          RL.fromList (replicate (deltaLine - 1) EmptyLine) `RL.snoc`
          IndependentComment (tokComment comment)

sortCommentedImports
  :: forall a.
     [CommentedImport [] a (ImportDecl GhcPs)]
  -> [CommentedImport [] a (ImportDecl GhcPs)]
sortCommentedImports =
  concatMap unpackImports . concatMap mergeGroups . map (fmap (sortGroups)) . groupify
  where
    unpackImports
      :: CommentedImport [] a (ImportDecl GhcPs)
      -> [CommentedImport [] a (ImportDecl GhcPs)]
    unpackImports = \case
      l@EmptyLine -> [l]
      l@IndependentComment{} -> [l]
      ImportStatement r ->
        map IndependentComment (isdCommentsBefore r) ++ [ImportStatement r]
    mergeGroups
      :: Either (CommentedImport [] a (ImportDecl GhcPs)) [ImportStatementData [] a (ImportDecl GhcPs)]
      -> [CommentedImport [] a (ImportDecl GhcPs)]
    mergeGroups = \case
      Left  x -> [x]
      Right y -> ImportStatement <$> y
    sortGroups
      :: [ImportStatementData [] a (ImportDecl GhcPs)]
      -> [ImportStatementData [] a (ImportDecl GhcPs)]
    sortGroups =
      List.sortOn (moduleNameString . unLoc . ideclName . isdImport)
    groupify
      :: [CommentedImport [] a (ImportDecl GhcPs)]
      -> [Either (CommentedImport [] a (ImportDecl GhcPs)) [ImportStatementData [] a (ImportDecl GhcPs)]]
    groupify cs = go [] cs
      where
        go :: [ImportStatementData [] a (ImportDecl GhcPs)]
           -> [CommentedImport [] a (ImportDecl GhcPs)]
           -> [Either (CommentedImport [] a (ImportDecl GhcPs)) [ImportStatementData [] a (ImportDecl GhcPs)]]
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
