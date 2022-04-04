{-# LANGUAGE FlexibleContexts #-}
module NewVar where

import Control.Monad.State
import Data.Char (ord, chr)

import TypeAST

-- TODO clean this up later
newVar :: MonadState Int m => m Name
newVar = do
  v <- get
  let nv = v + 1
  put nv
  let prime var' 0 = var'
      prime var' n = prime (var' ++ "'") (n - 1)
  let primes = nv `div` 26
  let var = [chr (ord 'a' + nv `mod` 26)]
  return $ prime var primes
