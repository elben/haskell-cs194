module Monad where

-- Hide monadic functions
import Prelude hiding ((>>=), return, (>>), Monad)

-- Playing around with Monad.

class Monad m where
  return :: a -> m a

  -- bind
  (>>=) :: m a -> (a -> m b) -> m b

  -- pass-through; ignore the first computed value
  (>>) :: m a -> m b -> m b
  (>>) m1 m2 = m1 >>= const m2
  -- Equiv:
  -- (>>) m1 m2 = (>>=) m1 (\_ -> m2)

instance Monad Maybe where
  return = Just
  -- Equiv:
  -- return a = Just a

  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) (Just a) f = f a
  (>>=) Nothing _ = Nothing

-- Examples of Maybe as monad:
maybeEx1 :: Maybe Int
maybeEx1 = Just 3 >>= (\x -> Just (x+1)) >>= (\x -> Just (x+2))

maybeEx2 :: Maybe Int
maybeEx2 = Just 3 >>= (\_ -> Nothing) >>= (\x -> Just (x+2))

maybeEx3 :: Maybe Int
maybeEx3 = return 3

maybeEx4 :: Maybe Int
maybeEx4 = Just 3 >> Just 4


instance Monad [] where
  return a = [a]

  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) as f = concatMap f as
  -- Equiv:
  -- (>>=) as f = concat $ map f as

listEx1 :: [Int]
listEx1 = return 3

listEx2 :: [Int]
listEx2 = [1, 2, 3, 4] >>= (\x -> [x, x*100])


-- Combine uses of Maybe and [] monads
maybeListEx1 :: [Int]
maybeListEx1 = case ((Just 3) >>= (\x -> Just [x, x])) of
               Just xs -> xs >>= (\x -> [x, x*100])
               Nothing -> []

maybeListEx2 :: [Int]
maybeListEx2 = case (Nothing >>= (\x -> Just [x, x])) of
               Just xs -> xs >>= (\x -> [x, x*100])
               Nothing -> []
