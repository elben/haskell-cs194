{-# LANGUAGE FlexibleInstances #-}
module Week05 where

-- Playing with parametric types

f1 :: a -> a
f1 a = a

f1' :: a -> a
f1' = f1

-- f2 :: a -> b
-- f2 a = a

f3 :: a -> b -> a
f3 a _ = a

f4 :: [a] -> [a]
f4 = take 4

f5 :: (b -> c) -> (a -> b) -> (a -> c)
f5 f g = f . g

f6 :: (a -> a) -> a -> a
f6 f = f

----------

class Listable a where
  toList :: a -> [Int]

class Personhood a where
  name :: a -> String
  age  :: a -> Int
  younger :: a -> a -> Bool


-------------

instance Listable Int where
  toList x = [x]

instance Listable Bool where
  toList True  = [1]
  toList False = [0]

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

sumL :: Listable a => a -> Int
sumL x = sum (toList x)


foo :: (Ord a, Listable a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y
