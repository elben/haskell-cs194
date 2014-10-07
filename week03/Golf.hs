module Golf where

-- qualified means we refer to the fns inside w/ a name.
-- http://www.haskell.org/haskellwiki/Import
import qualified Data.IntMap as IntMap

-- First item in returned list should be the original list.
-- Second item is every 2nd element from original list.
-- nth item is every nth element from original list.
--
-- Example:
--
-- skips "ABCDEF" == ["ABCDEF", "BDF", "CF", "D", "E", "F"]
skips :: [a] -> [[a]]
skips [] = []
skips xs = map (\i -> (skipsNth xs i)) [1..(length xs)]

-- Given a list and an int, return filtered list where each item is the nth item
-- from the original list.
skipsNth :: [a] -> Int -> [a]
skipsNth xs n = foldr
                  (\(x, i) acc -> if (mod i n) == 0 then x:acc else acc)
                  []
                  (zip xs [1..(length xs)])

-- We zip three version of the list, the first we drop the last two elements,
-- the second we drop one at the front and one in the back, and the third we
-- drop the first two elements. Like thsi:
--
-- Original:
-- [1 3 4 3 2 9 7 9 9 9]
--
--      [1 3 4 3 2 9 7 9] 9 9
--    1 [3 4 3 2 9 7 9 9] 9
--  1 3 [4 3 2 9 7 9 9 9]
--
-- This produces the zipped list:
-- [(1,3,4),(3,4,3),(4,3,2),(3,2,9),(2,9,7),(9,7,9),(7,9,9),(9,9,9)]
--
-- We can then look at each tuple and see if the middle item is greater than its
-- neighbors.
--
-- There's probably an easy strictly O(n) version of this (instead of O(2n) or
-- whatever this is, depending on laziness), but oh well.
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima xs = foldr
                   (\(a, b, c) acc -> if b > a && b > c then b:acc else acc)
                   []
                   (zip3 (take ((length xs) - 2) xs)
                        (take ((length xs) - 1) (drop 1 xs))
                        (drop 2 xs))


--------------
-- Histogram
--------------

firstLine :: String
firstLine = "==========\n0123456789\n"

-- TODO: need to iterate (maxValue m) times, starting from the bottom up.

histogram :: [Integer] -> String
histogram xs = snd (histogramHelp (collectHistogram xs))

histogramHelp :: IntMap.IntMap Integer -> (IntMap.IntMap Integer, String)
-- Strategy:
-- 1. At each iteration, draw "x" for all values remaining.
-- 2. Then, do an updateWithkey on the IntMap, which will delete the item if
--    there is no value left. Otherwise, decr the value by one.
--
-- A bit inefficient, since we do a lot of iterations over the k,v pairs. But oh
-- well.
histogramHelp m = foldl
                    (\acc _ -> (decrMap (fst acc), drawLine (fst acc) ++ (snd acc)))
                    (m, firstLine)
                    [0..((maxValue m)-1)]

-- Given IntMap, draws a singular line of *'s if the key exists for that
-- position in the string. An output coudl be "* *  *..." if the map contained
-- keys 0, 1, and 5.
drawLine :: IntMap.IntMap Integer -> String
drawLine m = (foldl (\acc x -> if IntMap.member x m then acc ++ "*" else acc ++ " ") "" [0..9]) ++ "\n"

-- Decrement value by one. If no value left, item is deleted from the map.
decrMap :: IntMap.IntMap Integer -> IntMap.IntMap Integer
decrMap m = foldl
              (\acc k -> (decrMapWithKey acc k))
              m (IntMap.keys m)

-- Given map and key, update key by decrementing value by one. If value <= 1,
-- key is removed from map.
decrMapWithKey :: IntMap.IntMap Integer -> IntMap.Key -> IntMap.IntMap Integer
decrMapWithKey m k = IntMap.updateWithKey (\_ v -> if v > 1 then Just (v-1) else Nothing) k m

-- Return the max value from the map.
maxValue :: IntMap.IntMap Integer -> Integer
maxValue m = IntMap.fold (\x acc -> max acc x) 0 m

-- Convert array of ints to IntMap, where keys are Int and values are number of
-- occurences of the key.
collectHistogram :: [Integer] -> IntMap.IntMap Integer
collectHistogram xs = foldl
                        (\acc x -> (IntMap.insert x ((IntMap.findWithDefault 0 x acc) + 1) acc))
                        IntMap.empty
                        (map fromInteger xs)
