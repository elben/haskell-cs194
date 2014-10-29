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

-----------------------------
-- Exercise 2: Binary tree
-----------------------------

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

-----------------------------
-- Exercise 3: More folds
-----------------------------


xor :: [Bool] -> Bool
xor xs = foldl (\acc x -> if x then not acc else acc) False xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x):acc) []

-- Implement myFoldl using only foldr
--
-- Strategy: composed functions right-to-left to be applied left-to-right. We
-- foldr a composed function, named comp. This composed function will eventually
-- take in the true accumulator (base, of type b), and return another b.
--
-- Similar to composing transducers in Clojure.
--
-- We begin with the `id` function, or (\acc -> acc).
--
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = (foldr (\x comp -> (\acc -> (f (comp acc) x))) id xs) base

-- Tests:
--
-- foldl (\acc x -> acc + x) 0 [1..10] == myFoldl (\acc x -> acc + x) 0 [1..10]

-----------------------------
-- Exercise 4: Finding primes
-----------------------------

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- Generate all odd prime numbers up to 2n + 2.
--
-- Strategy: Generate list of numbers to sieve out.
-- Then, for each number 0 to n: If the number is in the sieve list, remove it
-- from the sieve list, and don't add it to the primes list. If the number is
-- NOT in the sieve list, then 2x+1 to find the prime number.
-- Alternative strategy, make a list of primes backwards, and reverse it at the
-- end.

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = reverse $ snd $
  foldl
    (\(remaining, primes) x ->
      if x `elem` remaining
      then (Data.List.delete x remaining, primes)
      else (remaining, (2*x+1):primes))
  (sieve n, [])
  [1..n]

sieve :: Integer -> [Integer]
sieve n = [i + j + 2*i*j | i <- [0..n], j <- [0..n], 1 <= i && i <= j && i+j+(2*i*j) <= n]

-- Alternative strategy, does it backwards than the one above. There's a bug
-- with the sieve' method, because it spits out non-prime numbers.

sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = snd $
  foldr
    (\x (remaining, primes) ->
      if x `elem` remaining
      then (Data.List.delete x remaining, primes)
      else (remaining, (2*x+1):primes))
  (sieve' n, [])
  [1..n]

-- Return numbers to be sieved out of list, starting from highest number.
--
-- maxN is (n-1)/3, which is an optimization. We need to keep the invariant
-- i+j+2ij <= n and 1 <= i <= j. So to find the largest value of j possible,
-- assume i is 1. That means (i + j + 2ij) = (1 + j + 3j) = 3j + 1 <= n. Which
-- means j <= (n-1)/3. Hence why we start j there.
sieve' :: Integer -> [Integer]
sieve' n = let maxN = (n - 1) `div` 3
          in [i + j + 2*i*j | i <- [maxN,maxN-1..1],
                              j <- [i,i-1..1],
                              1 <= i && i <= j && i+j+(2*i*j) <= n]

