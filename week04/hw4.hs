module Hw4 where

import qualified Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = Data.List.foldl' (\acc x -> if even x then (x-2) * acc else acc) 1 xs


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = sum $ filter even (takeWhile (\m -> m /= 1) (iterate (\m -> if even m then m `div` 2 else 3 * m + 1) n))

checkFun2 :: Integer -> Bool
checkFun2 n = fun2 n == fun2' n

--------------------
-- Binary tree
--------------------

-- A Tree of a's. Integer is the height of tree.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Given a list of a's, return a balanced tree of a's.
-- Implement by using foldr.
foldTree :: [a] -> Tree a
foldTree as = foldr insertTree
                    Leaf
                    as

insertTree :: a -> Tree a -> Tree a
insertTree a Leaf = Node 0 Leaf a Leaf

-- If L is a leaf (R presumably a Leaf, since we always try L first), insert left.
-- We have to add a new node, so height increases.
insertTree a (Node h Leaf p r) = Node (h+1) (Node 0 Leaf a Leaf) p r

-- If L is a node, and R is a leaf, insert right. Don't increase height, since L
-- already has a node.
insertTree a (Node h l p Leaf) = Node h l p (Node 0 Leaf a Leaf)

-- If both L and R are nodes, need to figure out which side has room to insert.
-- We also need to update this node's height, since we may add a new node below.
insertTree a (Node h l p r)
  -- L is shorter, so insert left.
  | heightOfNode l < heightOfNode r = Node h (insertTree a l) p r
  -- R is shorter, so insert right.
  | heightOfNode l > heightOfNode r = Node h l p (insertTree a r)
  -- Equal heights. Check to see if one of them has room. If not, create a new
  -- node on the left.
  | otherwise =
      let numL = numNodes l
          numR = numNodes r
      in if numL > numR
         then (Node h l p (insertTree a r)) -- Room on the right
         else let l' = insertTree a l       -- Either no room or some room on left; try it.
              in if heightOfNode l' == h    -- Was a new row added?
                 then (Node (h+1) l' p r)   --   If new node (row) added, incr height.
                 else (Node h l' p r)       --   There was some room; no height change.

heightOfNode :: Tree a -> Integer
heightOfNode (Node h _ _ _) = h

numNodes :: Tree a -> Integer
numNodes Leaf = 0
numNodes (Node _ Leaf _ r) = (numNodes r) + 1
numNodes (Node _ l _ Leaf) = (numNodes l) + 1
numNodes (Node _ l _ r) = (numNodes l) + (numNodes r) + 1

-- Tests
-- foldTree "FGHIJ"
-- foldTree "EFGHIJ"
-- 
--
-- foldTree "ABCDEFGHIJ"
--
--           J
--       I       H
--     G   E   F   D
--    C   A   B
--
--  Node 3
--    (Node 2
--      (Node 1
--        (Node 0 Leaf 'C' Leaf)
--        'G'
--        Leaf)
--      'I'
--      (Node 1
--        (Node 0 Leaf 'A' Leaf)
--        'E'
--        Leaf))
--    'J'
--    (Node 2
--      (Node 1
--        (Node 0 Leaf 'B' Leaf)
--        'F'
--        Leaf)
--      'H'
--      (Node 0 Leaf 'D' Leaf))
-- 
--
-- foldTree "XYABCDEFGHIJ"
--
--           J
--       I       H
--     G   E   F   D
--    C X A   B   Y
--
-- Node 3
--   (Node 2
--     (Node 1 (Node 0 Leaf 'C' Leaf) 'G' (Node 0 Leaf 'X' Leaf))
--     'I'
--     (Node 1 (Node 0 Leaf 'A' Leaf) 'E' Leaf))
--   'J'
--   (Node 2
--     (Node 1 (Node 0 Leaf 'B' Leaf) 'F' Leaf)
--     'H'
--     (Node 1 (Node 0 Leaf 'Y' Leaf) 'D' Leaf))
--
--     
-- 
-- 

