module MonoidFun where

-- http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html

import Data.Monoid
import Data.Foldable
-- import Control.Monad.Writer
-- import Control.Monad.State


data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f r `mappend` f x `mappend` foldMap f l

tree :: Tree Integer
tree = Node (Leaf 1) 7 (Node (Leaf 100) 2 (Leaf (-3)))

anytest :: Any
anytest = Any True

ex1 :: Any
-- ex1 = foldMap (Any . (== 1)) tree
--               ^ is equvalent to:
ex1 = foldMap (\n -> Any (n == 1)) tree

ex2 :: All
ex2 = foldMap (All . (> 5)) tree

-- Exercise: write function to find min/max element of tree. Need to write new
-- monoid instance

data Max = Bottom
         | Max Integer
  deriving (Show, Eq)

getMax :: Max -> Maybe Integer
getMax (Max n) = Just n
getMax Bottom = Nothing

-- newtype Max = Max { getMax :: Integer }
instance Monoid Max where
  mempty = Bottom
  mappend x Bottom = x
  mappend Bottom y = y
  mappend (Max x) (Max y) = Max (max x y)

maxtest :: Max
maxtest = Max 3

ex3 :: Max
ex3 = foldMap Max tree
-- Equivalent:
-- ex3 = foldMap (\n -> Max n) tree

