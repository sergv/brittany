{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.Brittany.Internal.PreludeUtils (putStrErrLn) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Strict.Maybe qualified as Strict
import System.IO

instance Applicative Strict.Maybe where
  pure = Strict.Just
  Strict.Just f <*> Strict.Just x = Strict.Just (f x)
  _ <*> _ = Strict.Nothing

instance Monad Strict.Maybe where
  Strict.Nothing >>= _ = Strict.Nothing
  Strict.Just x >>= f = f x

instance Alternative Strict.Maybe where
  empty = Strict.Nothing
  x <|> Strict.Nothing = x
  _ <|> x = x

putStrErrLn :: MonadIO m => String -> m ()
putStrErrLn = liftIO . hPutStrLn stderr
