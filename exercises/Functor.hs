module Functor where

-- https://wiki.haskell.org/Typeclassopedia#Functor
-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids
-- http://www.seas.upenn.edu/~cis194/lectures/04-typeclasses.html

-- Hide Prelude's functors
import Prelude hiding (fmap, Functor)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

-- Examples:
--
-- fmap id [1,2,3]
-- fmap (+1) [1,2,3]

instance Functor Maybe where
  -- Use Maybe monad
  fmap f ma = do
    a <- ma
    return $ f a

-- Examples:
--
-- fmap (+1) (Just 3)
-- fmap (+1) Nothing

-- If a tuple has type constructor (,), then ((,) a) is a tuple where the first
-- element is of type a. A Functor only accepts one type variable, so we must
-- partially apply the tuple as ((,) a), so that the first element is of type a.
instance Functor ((,) a) where
  fmap f (a, b) = (a, f b)

-- Examples:
--
-- fmap (++ "3") (1, "2")
-- => (1, "23") :: Num t => (t, String)

---------------------
-- "Lifting"
---------------------
--
-- We can think of Functors also as "lifting" a function (a -> b) from the
-- normal world into the "f" world:
--
-- fmap :: (a -> b) -> f a -> f b
--
-- But when curried, we can think of it as:
--
-- fmap :: (a -> b) -> (f a -> f b)
--
-- So the function (a -> b) is "lifted" into the "f" world, producing (f a -> f b).

-- Here, -> is also treated as a type constructor that is partially applied. We
-- know the argument is of type `r`, but the return value us unknown in the
-- constructed type. Again, we must partially apply for Functor since Functor
-- only accepts one type var.
instance Functor ((->) r) where
  -- f :: (a -> b)
  -- g :: (r -> a)
  -- fmap's type tells us this must return function of type (r -> b):
  -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
  fmap f g = \r -> f (g r)
  -- This is equivalent to:
  -- fmap f g = f . g.
  -- Which is function composition! So a function's Functor acts like function
  -- composition.

-- Examples:
-- (fmap (+1) (*3)) 10
-- => 31
-- ((+1) . (*3)) 10
-- => 31

-- IO is also a Functor. In essense, to fmap over an IO type is to:
--
-- 1. Do the IO that return a value, then
-- 2. Call f on that returned value, returning another value.
instance Functor IO where
  -- fmap :: (a -> b) -> IO a -> IO b
  fmap f io = do
    a <- io
    return $ f a

-- Examples:
-- fmap reverse getLine
--
-- 1. Get line from user
-- 2. Reverse the string from user

-------------------
-- How to think about Functors
-------------------
--
-- The easy way to think about functors is to think of them as 'mappable'. You
-- can map over lists, you can 'map' over Maybe and Tuples.
--
-- The more general abstraction is thinking of Functors as a "computational
-- context". For example, with the IO and (r -> ) functors, when we 'fmap f o'
-- over them, we are doing some computation 'f' on the context of 'o'.
--
-- For IO, we can think of 'o' as the context (namely, the IO that's happening)
-- that 'f' is being applied on. For (r -> ), consider the example `(fmap (+1)
-- (*3)) 10`. We are doing a computation (+1) in the 'context' of (*3).

-------------------
-- Functor Laws
-------------------
-- Useful functors hold two laws to make sure that `fmap g` does not change the
-- structure of the container, but only the elements inside. (e.g. trees, lists)
--
-- (1) fmap id = id
-- (2) fmap (g . h) = (fmap g) . (fmap h)
--
-- (1) shows that mapping id over the elements have no effect (i.e. same as
-- calling id on the container). This also shows that fmap didn't do any 'extra'
-- secret to our functor work. E.g. this is an invalid implementation:
-- fmap f num = f (num + 1), since ((fmap id) 0) != id 0
--
-- (2) shows that running two fmaps with two functions on container is the same
-- as running fmap once using the composed function (g . h).
--
-- This makes implementations that modify the structure invalid:
--
-- fmap f list = map f (reverse list), since:
--
-- fmap ((+1) . (*3)) [1, 2, 3] != ((fmap (+1)) . (fmap (*3))) [1, 2, 3]
--
invalidFmap :: (a -> b) -> [a] -> [b]
invalidFmap f list = map f (reverse list)
-- invalidFmap ((+1) . (*3)) [1, 2, 3]
-- => [4, 7, 10]
-- ((invalidFmap (+1)) . (invalidFmap (*3))) [1, 2, 3]
-- => [10, 7, 4]
--

