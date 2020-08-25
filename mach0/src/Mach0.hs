{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
-- | Machine 0: a first (zeroth) experiment in building a stack machine
-- | Also a pun implying something slow.
module Mach0
  ( Mach0 -- abstract type
  , MachSt -- abstract type
  , values, instrs
  , pile, push, pop, swap
  , write, mark, erase
  , freeze--, thaw
  , load, save
  , fill
  ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

data MachSt s t =
  MachSt { stack :: [s]
         , sidestack :: [s]
         , tape :: [t]
         }

instance Semigroup (MachSt s t) where
  n <> m =
    MachSt { stack = stack n <> stack m
           , sidestack = sidestack n <> sidestack m
           , tape = tape n <> tape m
           }

instance Monoid (MachSt s t) where
  mempty = MachSt { stack = []
                  , sidestack = []
                  , tape = []
                  }

values :: [s] -> MachSt s t
values xs = mempty { stack = xs }

instrs :: [t] -> MachSt s t
instrs xs = mempty { tape = xs }

newtype Mach0 s t m a =
  Mach0 (StateT (MachSt s t) (ExceptT (MachSt s t) m) a)
  deriving (Functor, Applicative, Monad)
       via (StateT (MachSt s t) (ExceptT (MachSt s t) m))

fill :: Monad m => MachSt s t -> Mach0 s t m ()
fill st = Mach0 $ modify (st <>)

pile :: Monad m => [s] -> Mach0 s t m ()
pile xs = fill (values xs)

push :: Monad m => s -> Mach0 s t m ()
push x = pile [x]

freeze :: Monad m => Mach0 s t m a
freeze = Mach0 do
  st <- get
  lift $ throwE st

pop :: Monad m => Mach0 s t m s
pop = do
  st <- Mach0 get
  case stack st of
    x : xs -> Mach0 do
      put $ st { stack = xs }
      pure x
    _ -> freeze

swap :: Monad m => Mach0 s t m ()
swap = do
  st <- Mach0 get
  case stack st of
    x : x' : xs -> Mach0 do
      put $ st { stack = x' : x : xs }
    _ -> freeze

write :: Monad m => [t] -> Mach0 s t m ()
write xs = fill (instrs xs)

mark :: Monad m => t -> Mach0 s t m ()
mark x = write [x]

erase :: Monad m => Mach0 s t m t
erase = do
  st <- Mach0 get
  case tape st of
    x : xs -> Mach0 do
      put $ st { tape = xs }
      pure $ x
    _ -> freeze

save :: Monad m => Mach0 s t m ()
save = do
  st <- Mach0 get
  case stack st of
    x : xs -> Mach0 do
      put $ st { stack = xs
               , sidestack = x : sidestack st
               }
    _ -> freeze

load :: Monad m => Mach0 s t m ()
load = do
  st <- Mach0 get
  case sidestack st of
    x : xs -> Mach0 do
      put $ st { stack = x : stack st
               , sidestack = xs
               }
    _ -> freeze
