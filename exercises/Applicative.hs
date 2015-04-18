module Applicative where

-- import Prelude hiding (pure)


-- The lecture is from 2015 course, lecture week 8, Monads Part II.
--
-- http://www.seas.upenn.edu/~cis194/lectures/08-monads-ii.html

class Functor f => Applicative f where
  pure :: a -> f a

  -- Pronounced "apply", or "ap" for short
  (<*>)  :: f (a -> b) -> f a -> f b

  -- Also get:
  (<$>) :: (a -> b) -> f a -> f b
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
-- [id, (+1)] <*> [1,2,3]

instance Applicative Maybe where
  pure a = Just a

  (<*>) Nothing _ = Nothing
  (<*>) _ Nothing = Nothing
  (<*>) (Just f) (Just a) = Just $ f a

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
