module Golf where

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

