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
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE
import Language.Haskell.Brittany.Internal.Layouters.Import
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.GHC.ExactPrint as ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
  (DeltaPos(..), commentContents, deltaRow)

layoutModule :: ToBriDoc' HsModule
layoutModule lmod@(L _ mod') = case mod' of
    -- Implicit module Main
  HsModule{hsmodName = Nothing, hsmodImports} -> do
    commentedImports <- transformToCommentedImportM hsmodImports
    docLines (commentedImportsToDoc <$> sortCommentedImports commentedImports)
  HsModule{hsmodName = Just n, hsmodExports, hsmodImports} -> do
    commentedImports <- transformToCommentedImportM hsmodImports
    let tn = Text.pack $ moduleNameString $ unLoc n
    allowSingleLineExportList <-
      mAsk <&> _conf_layout .> _lconfig_allowSingleLineExportList .> confUnpack
    -- the config should not prevent single-line layout when there is no
    -- export list
    let allowSingleLine = allowSingleLineExportList || Data.Maybe.isNothing hsmodExports
    docLines
      $ docSeq
          [ docNodeAnnKW lmod Nothing docEmpty
             -- A pseudo node that serves merely to force documentation
             -- before the node
          , docNodeMoveToKWDP lmod AnnModule True $ runFilteredAlternative $ do
            addAlternativeCond allowSingleLine $ docForceSingleline $ docSeq
              [ appSep $ docLit $ Text.pack "module"
              , appSep $ docLit tn
              , docWrapNode lmod $ appSep $ case hsmodExports of
                Nothing -> docEmpty
                Just x -> layoutLLIEs True KeepItemsUnsorted x
              , docSeparator
              , docLit $ Text.pack "where"
              ]
            addAlternative $ docLines
              [ docAddBaseY BrIndentRegular $ docPar
                  (docSeq [appSep $ docLit $ Text.pack "module", docLit tn])
                  (docSeq
                    [ docWrapNode lmod $ case hsmodExports of
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

type CommentedImport' = CommentedImport (Comment, DeltaPos) (ImportDecl GhcPs)

instance Bifunctor ImportStatementData where
  bimap f g (ImportStatementData xs ys zs) =
    ImportStatementData (map f xs) (map f ys) (g zs)

transformToCommentedImportM
  :: [LImportDecl GhcPs] -> ToBriDocM [CommentedImport']
transformToCommentedImportM is = do
  (is' :: [(Maybe Annotation, ImportDecl GhcPs)]) <-
    for is $ \i@(L _ rawImport) -> do
      annotionMay <- astAnn i
      pure (annotionMay, rawImport)
  pure $ transformToCommentedImport is'

transformToCommentedImport
  :: [(Maybe Annotation, ImportDecl GhcPs)] -> [CommentedImport']
transformToCommentedImport is = do
  concat $ concatMap convertComment finalAcc : finalList
  where
    finalAcc  :: [(Comment, DeltaPos)]
    finalList :: [[CommentedImport']]
    (finalAcc, finalList) = mapAccumR accumF [] is

    convertComment :: (Comment, DeltaPos) -> [CommentedImport']
    convertComment (c, DP (y, x)) =
      replicate (y - 1) EmptyLine ++ [IndependentComment (c, DP (1, x))]

    accumF
      :: [(Comment, DeltaPos)]
      -> (Maybe Annotation, ImportDecl GhcPs)
      -> ([(Comment, DeltaPos)], [CommentedImport'])
    accumF accConnectedComm (annMay, decl) = case annMay of
      Nothing ->
        ( []
        , [ ImportStatement ImportStatementData
              { isdCommentsBefore  = []
              , isdCommentsAfter   = []
              , isdImport = decl
              }
          ]
        )
      Just ann ->
        let (newAccumulator, priorComments') =
              List.span ((== 0) . deltaRow . snd) (annPriorComments ann)
            go
              :: [(Comment, DeltaPos)]
              -> [(Comment, DeltaPos)]
              -> ([CommentedImport'], [(Comment, DeltaPos)], Int)
            go acc []                        = ([], acc, 0)
            go acc (c1@(_,  DP (y, _)) : []) = ([], c1 : acc, y - 1)
            go acc (c1@(_,  DP (1, _)) : xs) = go (c1 : acc) xs
            go acc (   (c1, DP (y, x)) : xs) =
              ( concatMap convertComment xs ++ replicate (y - 1) EmptyLine
              , (c1, DP (1, x)) : acc
              , 0
              )
            blanksBeforeImportDecl = deltaRow (annEntryDelta ann) - 1
            (convertedIndependentComments, beforeComments, initialBlanks) =
              if blanksBeforeImportDecl == 0
              then go [] (reverse priorComments')
              else (concatMap convertComment priorComments', [], 0)
        in
          ( newAccumulator
          , convertedIndependentComments
          ++ replicate (blanksBeforeImportDecl + initialBlanks) EmptyLine
          ++ [ ImportStatement ImportStatementData
                 { isdCommentsBefore  = beforeComments
                 , isdCommentsAfter   = accConnectedComm
                 , isdImport = decl
                 }
             ]
          )

sortCommentedImports
  :: forall a b.
     [CommentedImport a (ImportDecl b)]
  -> [CommentedImport a (ImportDecl b)]
sortCommentedImports =
  unpackImports . mergeGroups . map (fmap (sortGroups)) . groupify
  where
    unpackImports
      :: [CommentedImport a (ImportDecl b)]
      -> [CommentedImport a (ImportDecl b)]
    unpackImports xs = xs >>= \case
      l@EmptyLine -> [l]
      l@IndependentComment{} -> [l]
      ImportStatement r ->
        map IndependentComment (isdCommentsBefore r) ++ [ImportStatement r]
    mergeGroups
      :: [Either (CommentedImport a (ImportDecl b)) [ImportStatementData a (ImportDecl b)]]
      -> [CommentedImport a (ImportDecl b)]
    mergeGroups xs = xs >>= \case
      Left x -> [x]
      Right y -> ImportStatement <$> y
    sortGroups
      :: [ImportStatementData a (ImportDecl b)]
      -> [ImportStatementData a (ImportDecl b)]
    sortGroups =
      List.sortOn (moduleNameString . unLoc . ideclName . isdImport)
    groupify
      :: [CommentedImport a (ImportDecl b)]
      -> [Either (CommentedImport a (ImportDecl b)) [ImportStatementData a (ImportDecl b)]]
    groupify cs = go [] cs
      where
        go :: [ImportStatementData a (ImportDecl b)]
           -> [CommentedImport a (ImportDecl b)]
           -> [Either (CommentedImport a (ImportDecl b)) [ImportStatementData a (ImportDecl b)]]
        go [] = \case
          l@EmptyLine : rest            -> Left l : go [] rest
          l@IndependentComment{} : rest -> Left l : go [] rest
          ImportStatement r : rest      -> go [r] rest
          []                            -> []
        go acc = \case
          l@EmptyLine : rest            -> Right (reverse acc) : Left l : go [] rest
          l@IndependentComment{} : rest ->
            Left l : Right (reverse acc) : go [] rest
          ImportStatement r : rest      -> go (r : acc) rest
          []                            -> [Right (reverse acc)]

commentedImportsToDoc :: CommentedImport' -> ToBriDocM BriDocNumbered
commentedImportsToDoc = \case
  EmptyLine            -> docLitS "" -- docEmpty will not produce emply line here
  IndependentComment c -> commentToDoc c
  ImportStatement r    -> docSeq
    (layoutImport (isdImport r) : map commentToDoc (isdCommentsAfter r))
 where
  commentToDoc (c, DP (_y, x)) = docLitS (replicate x ' ' ++ commentContents c)
