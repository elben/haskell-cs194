module Hw1 where

---------------------------------
-- Validating Credit Card Numbers
---------------------------------

toDigits    :: Integer -> [Integer]
toDigits d
  | d <= 0    = []

  -- Strip off the last digit at every recur.
  -- e.g. 1234, strip off 4, becomes 123.
  | otherwise = (toDigits ((d - (mod d 10)) `div` 10)) ++ [(mod d 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev d = reverse (toDigits d)

-- Double every other, starting from the right.
-- Example: doulbeEveryOther [8,7,6,5] = [16,7,12,5]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = reverse (doubleEveryOtherHelp (reverse xs))

-- Double every other, starting from the left.
-- Example: doulbeEveryOther [5,6,7,8] = [5,12,7,16]
doubleEveryOtherHelp :: [Integer] -> [Integer]
doubleEveryOtherHelp [] = []
doubleEveryOtherHelp [x] = [x]
doubleEveryOtherHelp (x:y:zs) = x : y*2 : doubleEveryOtherHelp zs

-- Sum each digit in each integer.
-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | x >= 10   = sumDigits (toDigits x)
  | otherwise = x
sumDigits (x:xs) = (sumDigits [x]) + (sumDigits xs)

validate :: Integer -> Bool
validate d = (sumDigits (doubleEveryOther (toDigits d))) `mod` 10 == 0


---------------------------------
-- The Towers of Hanoi
---------------------------------

type Peg = String
type Move = (Peg, Peg)

-- Move n dics from peg a to peg b (using peg c as temp).
-- The algorithm is simple. To move n discs from a to b:
-- 1. Move (n-1) discs a -> c using b as temp storage.
-- 2. Move the remaining top disc a -> b.
-- 3. Move (n-1) discs c -> b using a as temp storage.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)

-- Using four pegs, we can decrease the cost a lot. My attempt isn't optimal I
-- don't think. But it seems to be optimal for 1 to 4 discs at least. The
-- strategy I've employed this:
--
-- 1. Move (n-2) discs a -> d
-- 2. Move second-to-last disc a -> c
-- 3. Move last disc a -> b
-- 4. Move second-to-last disc c -> b
-- 5. Move (n-2) dsics d -> b
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 n a b c d = (hanoi4 (n-2) a d b c) ++ [(a, c), (a, b), (c, a)] ++ (hanoi4 (n-2) d b c a)

