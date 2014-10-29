module Week05 where

-- Playing with parametric types

f1 :: a -> a
f1 a = a

f1' :: a -> a
f1' a = f1 a

-- f2 :: a -> b
-- f2 a = a

f3 :: a -> b -> a
f3 a _ = a

f4 :: [a] -> [a]
f4 as = take 4 as

f5 :: (b -> c) -> (a -> b) -> (a -> c)
f5 f g = f . g

f6 :: (a -> a) -> a -> a
f6 f a = f a

----------

class Listable a where
  toList :: a -> [Int]

class Personhood a where
  name :: a -> String
  age  :: a -> Int
  younger :: a -> a -> Bool


