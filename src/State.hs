{-# LANGUAGE TupleSections, InstanceSigs #-}

module State where

import Data.Bifunctor (first)
import Data.Function ((&))

newtype State a = State (String -> (a, String))
modify :: (String -> String) -> State a -> State a
modify f (State s_as) = State (\s -> (fst . s_as $ s, f s))
runState :: String -> State a -> (a, String)
runState initStateVal (State s_as) = s_as initStateVal

instance Functor State where
  fmap :: (a -> b) -> State a -> State b
  fmap f (State s_as) = State (\str -> s_as str & first f)

instance Applicative State where
  pure :: a -> State a
  pure v = State (v,)
  (<*>) :: State (a -> b) -> State a -> State b
  (<*>) (State s_fs) (State s_as) = State (\s -> (fst (s_fs s) & \f -> f (fst (s_as s)), s))
