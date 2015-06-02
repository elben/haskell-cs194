module Applicative where

-- import Prelude hiding (pure)


-- The lecture is from 2015 course, lecture week 8, Monads Part II.
--
-- http://www.seas.upenn.edu/~cis194/lectures/08-monads-ii.html
--
-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids
-- https://wiki.haskell.org/Typeclassopedia#Applicative
-- http://en.wikibooks.org/wiki/Haskell/Applicative_Functors

-- To understand Applicative functors, first understand Functors.


-- Compare the applicative functor type class with functor:
--
--
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a

  -- Pronounced "apply", or "ap" for short
  (<*>)  :: f (a -> b) -> f a -> f b
-- Functor:
--  fmap ::   (a -> b) -> f a -> f b

-- Also get the infix <$>, which is an alias for fmap.
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

instance Applicative [] where
  pure a = [a]

  -- h is a list of functions of type (a -> b).
  -- fa is a list of as.
  --
  -- So we need to do the product of h and fa, to get all
  -- possible results of b.
  (<*>) (h:hs) fa = fmap h fa ++ hs <*> fa
  (<*>) [] _ = []

-- Examples:
--
-- pure 4 :: [Int]
-- [id, (+100)] <*> [1,2,3]
-- => [1, 2, 3, 101, 102, 103]
--
-- :t pure (+3)
-- => (Applicative f, Num a) => f (a -> a)
--
-- pure (+3) <*> Just 1
-- => Just 4
--
-- pure (+3) <*> [1, 2, 3]
-- => [4, 5, 6]
--
-- (++) <$> ["hello", "world"] <*> [".", "!"]
-- => ["hello.","hello!","world.","world!"]
-- In above, the first part (++) <$> ["hello", "world"] turns into partially
-- applied functions: ["hello" ++, "world" ++]. This is then passed into the <*>
-- part.

instance Applicative Maybe where
  pure a = Just a

  (<*>) Nothing _ = Nothing
  (<*>) _ Nothing = Nothing
  (<*>) (Just f) (Just a) = Just $ f a

  -- Alternative implementation using fmap:
  -- (<*>) (Just f) ma = fmap f ma

  -- Alternative implementation using maybe monad:
  -- (<*>) h fa = do
  --   f <- h
  --   a <- fa
  --   return $ f a

-- Examples:
--
-- pure 4 :: Maybe Int
-- Just (+1) <*> pure 4
-- Just (+1) <*> Just 4
--
-- Nothing <*> Just 3
-- => Nothing

instance Applicative IO where
  pure = return

  -- (<*>)  :: f (a -> b) -> f a -> f b
  f <*> fa = do
    f' <- f
    a <- fa
    return $ f' a

-- Examples of Applicative IO:
--
-- main = do
--   twoLines <- (++) <$> getLine <*> getLine
--   print twoLines

--------------------------------------------------
-- When Applicative is more useful than Functors.
--------------------------------------------------
--
-- With a functor, you can only fmap a function that operates on the value
-- inside the context. You also can't get the value out of the context in a
-- general way:
--
-- fmap :: (a -> b) -> f a -> f b
--
-- (+) `fmap` Just 1. <--- This works (we get a partial function (+1) inside a
-- Just), but what if we want to continue doing things to the result of it, like
-- give it the second argument? We can't do another `fmap` like this:
-- (+) `fmap` Just 1 `fmap` Just 3 => 4. Won't work!! We have to pattern match
-- explicitly on the Maybe--it's not general for all data types.
--
-- Applicative, however, gives us the power to do this!
--
-- let x = Just 1
-- let y = Just 3
-- (+) <$> x <*> y
-- => Just 4
--
-- let x = Just 1
-- let y = Nothing
-- (+) <$> x <*> y
-- => Nothing
--
--
-- We can also do:
-- pure (+) <*> Just 1 <*> Just 3
--
-- This shows that (pure (+) Just 1) is equivalent to ((+) <$> Just 1). In fact,
-- this is one of the Applicative laws.

type Name = String
data Employee = Employee { name :: Name, phone :: String }
  deriving Show

-- Examples:
--
-- Employee <$> Just "Elben" <*> Just "55555555"
-- Employee <$> Nothing <*> Just "55555555"
-- Employee <$> Just "Elben" <*> Nothing
--
-- Same as this derivation:
--
--                        f (a -> b) <*> f a
-- 1.    (Employee <$> Just "Elben") <*> Just "55555555"
-- 2. (fmap Employee (Just "Elben")) <*> Just "55555555"
-- 3.      (Just (Employee "Elben")) <*> Just "55555555"

