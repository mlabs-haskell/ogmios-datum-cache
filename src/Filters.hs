{-# OPTIONS_GHC -Wno-orphans #-}

module Filters (Semiring (..), Filter) where

type Filter f = f -> Bool

class Monoid m => Semiring m where
  (<^>) :: m -> m -> m

  srconcat :: [m] -> m
  srconcat = foldr (<^>) mempty

instance Semiring b => Semiring (a -> b) where
  f <^> g = \x -> f x <^> g x

instance Semigroup Bool where
  (<>) = (&&)

instance Monoid Bool where
  mempty = False

instance Semiring Bool where
  (<^>) = (||)