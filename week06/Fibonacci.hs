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

-- A data type representing an infinite stream by only having Cons, no Empty.
data Stream a = Cons a (Stream a)

-- Calls show on the stream N times, provided that the items implement Show.
showStreamNTimes :: (Show a) => Stream a -> Int -> String
showStreamNTimes _ 0 = ""
showStreamNTimes (Cons a s) n = show a ++ " " ++ showStreamNTimes s (n-1)

-- Given that 'a' implements Show, implement Show (Stream a)
instance Show a => Show (Stream a) where
  -- TODO: show first 20 elems, instead of just first one
  -- show (Cons a (Cons b s)) = show a ++ show b
  show s = showStreamNTimes s 20

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

-- Generates stream of repeated copies.
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

-- Applies function to every element in stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

-- Generates a Stream from a “seed” of type a, which is the first element of the
-- stream, and an “unfolding rule” of type a -> a which specifies how to
-- transform the seed into a new seed, to be used for generating the rest of the
-- stream.
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f (f a)

-- Stream of natural numbers
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- 0 0 0 0 ...
zeros :: Stream Integer
zeros = streamRepeat 0

-- 2 4 6 8 ...
evensOnly :: Stream Integer
evensOnly = streamFromSeed (+2) 0

-- Interleaves two streams
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a s1) (Cons b s2) = Cons a (Cons b $ interleaveStreams s1 s2)

-- Stream where nth element (first element is n = 1) is the largest power of 2
-- which evenly divides n.
--
-- Example (stream goes down):
-- element             n
-- 0 since 2^0 divides 1
-- 1 since 2^1 divides 2
-- 0 since 2^0 divides 3
-- 2 since 2^2 divides 4
-- 0 since 2^0 divides 5
-- 1 since 2^1 divides 6
-- 0 since 2^0 divides 7
-- 3 since 2^3 divides 8
--              n: 1  2     4     6     8    10    12    14    16          20          24
-- First few runs: 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3
--
-- How to implement: interleave zero stream with some streamFromSeed stream?
-- What is the second stream. It's non-1 if n is divisible by a power of 2.
--
-- ruler :: Stream Integer
-- ruler = nats


