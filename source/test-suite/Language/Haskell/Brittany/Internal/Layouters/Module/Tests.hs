----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Brittany.Internal.Layouters.Module.Tests
-- Copyright   :  (c) Sergey Vinokurov 2022
----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.Brittany.Internal.Layouters.Module.Tests (tests) where

import Prelude hiding (mod)

import Data.Bifunctor
import qualified Data.Map.Strict as M
import Test.Tasty
import Test.Tasty.HUnit

import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Unit.Module.Name

import Language.Haskell.Brittany.Internal.Layouters.Module
import Language.Haskell.Brittany.Internal.ParseModule
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint

transformToCommentedImportSimplified
  :: ExactPrint.Anns
  -> [LImportDecl GhcPs]
  -> [CommentedImport String ModuleName]
transformToCommentedImportSimplified anns is
  = map (bimap (ExactPrint.commentContents . fst) (unLoc . ideclName))
  $ transformToCommentedImport
  $ map (\x -> (M.lookup (ExactPrint.mkAnnKey x) anns, unLoc x)) is

-- Morally pure... (who believes this anyway?)
parseImports :: String -> IO (ExactPrint.Anns, [LImportDecl GhcPs])
parseImports source = do
  res <- parseModuleFromString [] "<brittany test suite input>" (\_ -> pure (Right ())) source
  case res of
    Left err -> error err
    Right (anns, mod, ()) -> pure (anns, hsmodImports (unLoc mod))

instance Show ModuleName where
  show = moduleNameString

tests :: TestTree
tests = testGroup "transformToCommentedImport"
  [ testCase "test 1" $
      "import Foo\n\
      \import Bar"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = [] }
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = [], isdCommentsAfter = [] }
      ]
  , testCase "test 2" $
      "import Foo\n\
      \\n\
      \\n\
      \import Bar"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = [] }
      , EmptyLine
      , EmptyLine
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = [], isdCommentsAfter = [] }
      ]
  , testCase "test 3" $
      "import Foo\n\
      \-- aa\n\
      \import Bar"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = [] }
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = ["-- aa"] , isdCommentsAfter = [] }
      ]
  , testCase "test 4" $
      "import Foo\n\
      \\n\
      \-- aa\n\
      \import Bar"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = [] }
      , EmptyLine
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = ["-- aa"] , isdCommentsAfter = [] }
      ]
  , testCase "test 5" $
      "import Foo\n\
      \\n\
      \-- aa\n\
      \\n\
      \import Bar"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = [] }
      , EmptyLine
      , IndependentComment "-- aa"
      , EmptyLine
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = [], isdCommentsAfter = [] }
      ]
  , testCase "test 6" $
      "import Foo\n\
      \-- aa\n\
      \\n\
      \import Bar"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = [] }
      , IndependentComment "-- aa"
      , EmptyLine
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = [], isdCommentsAfter = [] }
      ]
  , testCase "test 7" $
      "import Foo\n\
      \\n\
      \-- aa\n\
      \\n\
      \import Bar\n\
      \-- bb\n"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = [] }
      , EmptyLine
      , IndependentComment "-- aa"
      , EmptyLine
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = [], isdCommentsAfter = [] }
      ]
  , testCase "test 8" $
      "import Foo\n\
      \\n\
      \-- aa\n\
      \\n\
      \import Bar\n\
      \\n\
      \-- bb\n"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = [] }
      , EmptyLine
      , IndependentComment "-- aa"
      , EmptyLine
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = [], isdCommentsAfter = [] }
      ]
  , testCase "test 9" $
      "import Foo\n\
      \\n\
      \-- aa\n\
      \-- bb\n\
      \\n\
      \import Bar\n\
      \\n\
      \-- bb\n"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = [] }
      , EmptyLine
      , IndependentComment "-- aa"
      , IndependentComment "-- bb"
      , EmptyLine
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = [], isdCommentsAfter = [] }
      ]
  , testCase "test 10" $
      "import Foo\n\
      \\n\
      \\n\
      \-- aa\n\
      \-- bb\n\
      \import Bar\n"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = [] }
      , EmptyLine
      , EmptyLine
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = ["-- aa", "-- bb"], isdCommentsAfter = [] }
      ]
  , testCase "test 11" $
      "import Foo -- aa\n\
      \import Bar\n"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = ["-- aa"] }
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = [], isdCommentsAfter = [] }
      ]
  , testCase "test 12" $
      "import Foo {- aa -}\n\
      \import Bar\n"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = ["{- aa -}"] }
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = [], isdCommentsAfter = [] }
      ]
  , testCase "test 13" $
      "import Foo {- aa -} {- bb -}\n\
      \import Bar\n"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = ["{- aa -}", "{- bb -}"] }
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = [], isdCommentsAfter = [] }
      ]
  , testCase "test 14" $
      "import Foo {- aa -} {- bb -}\n\
      \-- cc\n\
      \import Bar\n"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = ["{- aa -}", "{- bb -}"] }
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = ["-- cc"], isdCommentsAfter = [] }
      ]
  , testCase "test 15" $
      "import Foo {- aa -} {- bb -}\n\
      \{- cc -}\n\
      \import Bar\n"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = ["{- aa -}", "{- bb -}"] }
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = ["{- cc -}"], isdCommentsAfter = [] }
      ]
  , testCase "test 16" $
      "import Foo {- aa -} {- bb -}\n\
      \\n\
      \{- cc -}\n\
      \\n\
      \import Bar\n"
      ==>
      [ ImportStatement ImportStatementData { isdImport = mkModuleName "Foo" , isdCommentsBefore = [], isdCommentsAfter = ["{- aa -}", "{- bb -}"] }
      , EmptyLine
      , IndependentComment "{- cc -}"
      , EmptyLine
      , ImportStatement ImportStatementData { isdImport = mkModuleName "Bar" , isdCommentsBefore = [], isdCommentsAfter = [] }
      ]
  ]
  where
    (==>) :: String -> [CommentedImport String ModuleName] -> Assertion
    (==>) src expected = do
      (anns, imports) <- parseImports src
      let actual = transformToCommentedImportSimplified anns imports
      assertEqual "Trasformed imports differ" expected actual

