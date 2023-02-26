{-# LANGUAGE TupleSections, InstanceSigs #-}

module State where

import Data.Bifunctor (first)
import Data.Function ((&))

newtype State s a = State (s -> (a, s))
modify :: (s -> s) -> State s a -> State s a
modify f (State s_as) = State (\s -> (fst . s_as $ s, f s))
runState :: s -> State s a -> (a, s)
runState initStateVal (State s_as) = s_as initStateVal

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State s_as) = State (\str -> s_as str & first f)

instance Applicative (State s) where
  pure :: a -> State s a
  pure v = State (v,)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) (State s_fs) (State s_as) = State (\s -> (fst (s_fs s) & \f -> f (fst (s_as s)), s))
