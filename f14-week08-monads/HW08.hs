module HW08 where

import Data.Maybe
import Data.Monoid
import qualified Text.Read as R
import qualified Data.List as L
import Control.Monad
import Control.Monad.Random

----------------
-- Exercise 1
----------------

-- Write a function that detects whether or not a string has a certain format.
-- The required format is as follows:
--
-- 1. The string starts with a digit.
-- 2. Say the value of this digit is n. The string next contains n as.
-- 3. After the n as, either the string ends or the sequence repeats, starting
--    with a (perhaps different) digit.
--
-- Examples:
--
-- 3aaa2aa
-- 4aaaa
-- 0
-- 001a
--
-- Use the Maybe monad.

-- Hint: Use readMaybe :: Read a => String -> Maybe a from
-- Text.Read and stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
-- from Data.List.


-- In the base case, the empty string follows the format.
--
-- For the non-empty String case, the strategy is to use three transforms:
--
-- 1. Try to read the head of the String as an Int. If successful, we get back
--    Just 3 or whatever. Bind to next transfrom (>>=).
--
-- 2. See if the tail of the String has n 'a's. e.g. if we previous got Just 3,
--    then we should be able to strip "aaa". If we have to little 'a's, fail. If
--    enough, continue.
--
-- 3. Recurse. So if we had *too* many 'a's (e.g. "3aaaaa"), then we would
--    recurse and fail in at (1).
--
stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
  where go :: String -> Maybe String
        go "" = Just ""
        -- go s = (R.readMaybe [head s] :: Maybe Int) >>=
        --        \n -> L.stripPrefix (concat (replicate n "a")) (tail s) >>=
        --        \remaining -> go remaining
        --
        -- The do notation below is equivalent to above.
        go s = do
               n <- R.readMaybe [head s] :: Maybe Int
               remaining <- L.stripPrefix (concat (replicate n "a")) (tail s)
               go remaining

-- Examples:
ex1 :: Bool
ex1 = stringFitsFormat "000"

ex2 :: Bool
ex2 = stringFitsFormat "01a1a"

ex3 :: Bool
ex3 = stringFitsFormat "3aaa"

ex4 :: Bool
ex4 = not $ stringFitsFormat "13aaaaaaaaaaaaa"

ex5 :: Bool
ex5 = stringFitsFormat "2aa3aaa0001a"

ex6 :: Bool
ex6 = stringFitsFormat ""

ex7 :: Bool
ex7 = not $ stringFitsFormat "001"

ex8 :: Bool
ex8 = not $ stringFitsFormat "1a004aaa3aaa"

allExamples :: Bool
allExamples = ex1 && ex2 && ex3 && ex4 && ex5 && ex6 && ex7 && ex8

----------------
-- Exercise 2
----------------

-- Use a list comprehension to produce the list of all numbers between
-- 1 and 100 (inclusive) that are divisible by 5 but not by 7.
--
-- specialNumbers :: [Int]

----------------
-- Risk, the game
----------------

-- Rand is the monad data type.
-- StdGen is the random number generator we want to use.
type StdRand = Rand StdGen

type Army = Int
data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army }
  deriving Show

type DieRoll = Int

instance Monoid ArmyCounts where
  -- mempty :: ArmyCount
  mempty = ArmyCounts 0 0
  -- mappend :: ArmyCount -> ArmyCount -> ArmyCount
  mappend a1 a2 = ArmyCounts (attackers a1 + attackers a2) (defenders a1 + defenders a2)

-- A monad. The random number generator is "passed along" via monad.
dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1, 7)

-- attacker rolls -> defender rolls -> army counts
battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults as ds = battleResults' (L.sortBy (flip compare) as) (L.sortBy (flip compare) ds)

-- Assumes [DieRoll] are sorted
battleResults' :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults' (a:as) (d:ds)
 | a > d     = ArmyCounts 0 (-1) <> armies
 | otherwise = ArmyCounts (-1) 0 <> armies
   where armies = battleResults' as ds
battleResults' _ _ = mempty

-- Examples:
--
-- battleResults [3,6,4] [5,5] == ArmyCounts { attackers = -1, defenders = -1 }
-- battleResults [3,6,4] [5,6] == ArmyCounts { attackers = -2, defenders = 0 }
-- battleResults [4] [3,2] == ArmyCounts { attackers = 0, defenders = -1 }

maxAttackers :: ArmyCounts -> Army
maxAttackers armies = min (attackers armies) 3

maxDefenders :: ArmyCounts -> Army
maxDefenders armies = min (defenders armies) 2

-- One step in battle. Assumes player/defender will use max number of armies
-- they can.
--
-- The monad we're using here is StdRand ArmyCounts, which can translate to
-- "keep the random number generator (StdGen) passed along, and also keep the
-- state of the army counts"
battle :: ArmyCounts -> StdRand ArmyCounts
battle armies = do
  -- sequences :: Monad m => [m a] -> m [a]
  -- aRolls <- sequence (replicate (maxAttackers armies) dieRoll)
  -- dRolls <- sequence (replicate (maxDefenders armies) dieRoll)
  --
  -- replicateM :: Monad m => Int -> m a -> m [a]
  --
  -- The value we get here is StdRand [DieRoll], which goes into
  -- aRolls :: [DieRoll].
  --
  -- Note that when we don't have 0 attackers or defenders, the battle doesn't
  -- happen. That is, (replicateM 0 dieRoll) will do no rolls, and thus we just
  -- return the current army.
  aRolls <- replicateM (maxAttackers armies) dieRoll
  dRolls <- replicateM (maxDefenders armies) dieRoll

  -- Return the ArmyCounts wrapped in the StdRand monad
  return $ armies <> (battleResults aRolls dRolls)

-- Monad:
-- return :: a -> m a
-- (>>=) :: m a -> (a -> m b) -> m b

-- Examples:
--
-- battle (ArmyCounts 10 10)

invade :: ArmyCounts -> StdRand ArmyCounts
invade armies
  | (attackers armies) < 2 = return armies
  | (defenders armies) == 0 = return armies
  | otherwise = (battle armies) >>= invade
  -- Equivalently:
  -- | otherwise = do
  --   remaining <- (battle armies)
  --   invade remaining

main = do
  values <- evalRandIO (invade (ArmyCounts 10 10))
  putStrLn (show values)
