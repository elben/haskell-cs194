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


