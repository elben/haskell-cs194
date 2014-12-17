module Fibonacci where

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

-- The idea is, to generate position n, to take two "lines", one is the position
-- n-1 and the other n-2, and sum these two position values to find value of
-- position n.
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3


