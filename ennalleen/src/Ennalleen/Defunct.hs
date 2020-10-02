{-# LANGUAGE GADTs #-}
module Ennalleen.Defunct where

import Ennalleen.Syntax
import Control.Monad.Writer
import Control.Monad.Free.Church (F, iterM)

-- | Function definition
data DefFun a = DefFun
  { funName :: Name
  , args :: [Name]
  , body :: a
  }

class NameGen m where
  uniqueName :: m Name

type WithDefFuns m a = WriterT [DefFun a] m a

-- | Uncurried function application
class InjectApplyN a where
  injectApplN :: a -> [a] -> a

defunct1 :: (InjectVar a, InjectApplyN a, NameGen m, Monad m)
         => LCFOAS (WithDefFuns m a) -> WithDefFuns m a
defunct1 (Lambda _arg mBody) = do
  _body <- mBody
  n <- lift uniqueName
  -- TODO uncurry
  -- TODO turn this lambda into a Defun
  return $ injectVar n
defunct1 (Apply mF mX) = do
  f <- mF
  x <- mX
  -- TODO uncurry f
  return $ injectApplN f [x]

defunctionalize :: (InjectVar a, InjectApplyN a, NameGen m, Monad m)
                => F LCFOAS a -> WithDefFuns m a
defunctionalize = iterM defunct1

-- | Defunctionalized expression
--data Defunct where
--  DVar :: Name -> Defunct
--  DInt :: Int -> Defunct
--  DBool :: Bool -> Defunct
--  DTimes :: Defunct -> Defunct -> Defunct
--  DPlus :: Defunct -> Defunct -> Defunct
--  DMinus :: Defunct -> Defunct -> Defunct
--  DEqual :: Defunct -> Defunct -> Defunct
--  DLess :: Defunct -> Defunct -> Defunct
--  DIf :: Defunct -> Defunct -> Defunct -> Defunct
--  DApply :: [Defunct] -> Defunct
--  DLet :: Name -> Defunct -> Defunct -> Defunct
