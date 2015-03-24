{-# LANGUAGE RankNTypes #-}

module Week09 where

import Test.QuickCheck
import Test.HUnit
import Data.List
import Control.Monad

x :: Int
x = 1

-----------------------------
-- HUnit
-----------------------------

merge1 :: Ord a => [a] -> [a] -> [a]
merge1 (x:xs) (y:ys)
  | x <  y    = x : merge1 xs ys
  | otherwise = y : merge1 xs ys
merge1 _ _    = []

test1_merge1 :: Test
test1_merge1 = "alternating numbers: [1,3,5] [2,4,6]" ~:
               merge1 [1,3,5] [2,4,6] ~?= [1,2,3,4,5,6]

test2_merge1 :: Test
test2_merge1 = TestList [ "one element: [1] []" ~:
                          merge1 [1] [] ~?= [1],
                          test1_merge1 ]


type MergeFun = Ord a => [a] -> [a] -> [a]

merge2 :: MergeFun
merge2 all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge2 xs all_ys
  | otherwise = y : merge2 all_xs ys
merge2 _ _     = []

merge3 :: MergeFun
merge3 all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge3 all_ys xs
  | otherwise = y : merge3 all_xs ys
merge3 xs []     = xs
merge3 _ _     = []


test2 :: MergeFun -> Test
test2 merge = TestList [ "one element: [1] []" ~:
                          merge [1] [] ~?= [1],

                          "alternating numbers: [1,3,5] [2,4,6]" ~:
                          merge [1,3,5] [2,4,6] ~?= [1,2,3,4,5,6]

                       ]

test3 :: MergeFun -> Test
test3 merge = "empty lists: [] []" ~:
              merge [] [] ~?= ([] :: [Integer])

-----------------------------
-- QuickCheck
-----------------------------

prop_numElements_merge3 :: [Integer] -> [Integer] -> Bool
prop_numElements_merge3 xs ys
  = length xs + length ys == length (merge3 xs ys)

prop_numElements :: MergeFun -> [Integer] -> [Integer] -> Bool
prop_numElements merge xs ys
  = length xs + length ys == length (merge xs ys)

merge4 :: MergeFun
merge4 all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge3 all_ys xs
  | otherwise = y : merge3 all_xs ys
merge4 xs ys  = xs ++ ys

prop_sorted1 :: MergeFun -> [Integer] -> [Integer] -> Bool
prop_sorted1 merge xs ys
  = merge xs ys == sort (xs ++ ys)

-- Tell QuickCheck it must only use sorted lists
prop_sorted2 :: MergeFun -> [Integer] -> [Integer] -> Property
prop_sorted2 merge xs ys
  = isSorted xs && isSorted ys ==> merge xs ys == sort (xs ++ ys)

isSorted :: Ord a => [a] -> Bool
isSorted (a:b:rest) = a <= b && isSorted (b:rest)
isSorted _          = True -- fewer than 2 elements

-- Tell QuickCheck to *generate* sorted lists
prop_sorted3 :: MergeFun
             -> OrderedList Integer -> OrderedList Integer -> Bool
prop_sorted3 merge (Ordered xs) (Ordered ys)
 = merge xs ys == sort (xs ++ ys)

----------
-- Generating arbitrary data for our custom data types.
----------

data MyList a = Nil | Cons a (MyList a)

instance Show a => Show (MyList a) where
  show = show . toList

toList :: MyList a -> [a]
toList Nil = []
toList (a `Cons` as) = a : toList as

fromList :: [a] -> MyList a
fromList []     = Nil
fromList (a:as) = Cons a (fromList as)

genMyList1 :: Arbitrary a => Gen (MyList a)
genMyList1 = do
  len <- choose (0, 10)
  vals <- replicateM len arbitrary
  return $ fromList vals
-- Usage: sample (genMyList1 :: Gen (MyList Integer))

-- sized takes continuation on what to do with that chosen size.
genMyList3 :: Arbitrary a => Gen (MyList a)
genMyList3 = sized (\size -> do
  len <- choose (0, size)          -- choose a length
  vals <- replicateM len arbitrary -- choose len a's
  return $ fromList vals)          -- convert to MyList
-- Usage: sample (genMyList3 :: Gen (MyList Integer))

-- As one last example, we can also choose arbitrary generators from a list based
-- on frequency. Although the length method of genMyList3 works well for lists, the
-- following technique is much better for trees.
--
-- `frequency` takes a list of (Int, Gen a) pairs and produces a Gen a. The
-- numbers in the list give the likelihood of choosing that element. Above, we
-- fixed the frequency of Nil at 1, but let the likelihood of Cons vary
-- according to the desired size.
--
-- Then, in the recursive call to genMyList4, we used resize to lower the size
-- parameter. Otherwise, it’s too likely to get a runaway list that goes on
-- toward infinity.
--
genMyList4 :: Arbitrary a => Gen (MyList a)
genMyList4 = sized $ \size -> do
  frequency [ (1, return Nil)
            , (size, do x <- arbitrary
                        xs <- {- resize (size - 1) -} genMyList4
                        return (x `Cons` xs) )]


