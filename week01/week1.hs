module Week1 where

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

s :: String
s = "hello, haskell"

i :: Int
i = -78

-- n :: Integer
-- n = 6^666

sumotrial :: Integer -> Integer
sumotrial 0 = 0
sumotrial n = n + sumotrial (n-1)

hailstone :: Integer -> Integer
hailstone n
  | mod n 2 == 0 = div n 2
  | otherwise    = 3*n + 1

sumPair :: (Int, Int) -> Int
sumPair (x,y) = x + y

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (_:xs) = 1 + intListLength xs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n)
