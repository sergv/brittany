{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Obfuscation (obfuscate) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.List as L
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import System.Random

obfuscate :: Text -> IO Text
obfuscate input = do
  let p x    = isAlphaNum x || x == '_' || x == '\''
  let groups = L.groupBy (\a b -> p a && p b) (T.unpack input)
  let idents = S.toList $ S.fromList $ filter (all p) groups
  let exceptionFilter x | x `S.member` keywords = False
      exceptionFilter x | x `S.member` extraKWs = False
      exceptionFilter x = not $ null $ drop 1 x
  let filtered = filter exceptionFilter idents
  mappings <- fmap M.fromList $ filtered `forM` \x -> do
    r <- createAlias x
    pure (x, r)
  let groups' = groups <&> \w -> fromMaybe w (M.lookup w mappings)
  pure $ T.concat $ fmap T.pack groups'

keywords :: Set String
keywords = S.fromList
  [ "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "mdo"
  , "else"
  , "forall"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "qualified"
  , "then"
  , "type"
  , "where"
  , "_"
  , "foreign"
  , "ccall"
  , "as"
  , "safe"
  , "unsafe"
  , "hiding"
  , "proc"
  , "rec"
  , "family"
  ]

extraKWs :: Set String
extraKWs = S.fromList ["return", "pure", "Int", "True", "False", "otherwise"]

createAlias :: String -> IO String
createAlias xs = go NoHint xs
  where
    go _    []       = pure ""
    go hint (c : cr) = do
      c' <- case hint of
        VocalHint | isUpper c -> randomFrom upperExtra
        _         | isUpper c -> randomFrom upper
        VocalHint | isLower c -> randomFrom lowerExtra
        _         | isLower c -> randomFrom lower
        _                     -> pure c
      cr' <- go (if c' `S.member` vowels then NoVocalHint else VocalHint) cr
      pure (c' : cr')

    lower      = V.fromList ['a' .. 'z']
    upper      = V.fromList ['A' .. 'Z']
    lowerExtra = V.fromList "aaaeeeoooiiiuuu" <> lower
    upperExtra = V.fromList "AAAEEEOOOIIIUUU" <> upper
    vowels     = S.fromList "aeuioAEUIO"

data Hint = NoHint | VocalHint | NoVocalHint

randomFrom :: Vector a -> IO a
randomFrom l = do
  let hi = length l - 1
  gen <- getStdGen
  let (x, gen') = randomR (0, hi) gen
  setStdGen gen'
  pure $ l V.! x
