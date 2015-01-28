module FunctorFun where

import Control.Monad.Instances

-- Functor is defined as:
--
-- class Functor f where
--   fmap  :: (a -> b) -> f a -> f b
--
-- Where f is a type constructor like Maybe (instead of Nothing or Just _).

eitherLeft :: Either String Int
eitherLeft = Left "I'm a string"

eitherRight :: Either String Int
eitherRight = Right 100

ex1 :: Bool
ex1 = fmap (+10) eitherLeft == Left "I'm a string"

ex2 :: Bool
ex2 = fmap (+10) eitherRight == Right 110

ex3 :: Bool
ex3 = fmap (+1) [1, 2, 3] == [2, 3, 4]

-- Tuples implement functors for the type constructor ((,) e).
-- This can be thought of as (e,), or an "unfinished tuple declaration", where e
-- is a type var.
--
-- Since the implementation of the Functor instance for ((,) e) doesn't care
-- about the type e, it can be anything when it acts as the type constructor.
-- The type of the other part of the tuple (in the example below, Int) is
-- specified when the value ("Left", 100) is built.
--
-- instance Functor ((,) e) where
--  fmap f (x,y) = (x, f y)
--
-- Look at the instnace of Functor for a 3-tuple to see how this is generalized.
--
-- Create a list [100], ignores "Left" string.
ex4 :: (String, [Integer])
ex4 = fmap (\n -> [n]) ("Left", 100)

-- In this case, x :: u, y :: v. And the type var 'a' in the Functor declaration
-- (way above) is set to the type of z, since f :: a -> b.
instance Functor ((,,) u v) where
  fmap f (x,y,z) = (x, y, f z)

-- Answer: ("Left", 100, [200])
ex5 :: (String, Integer, [Integer])
ex5 = fmap (\n -> [n]) ("Left", 100, 200)

