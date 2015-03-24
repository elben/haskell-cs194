module Functor where

-- Hide Prelude's functors
import Prelude hiding (fmap, Functor)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

-- Examples:
--
-- fmap id [1,2,3]
-- fmap (+1) [1,2,3]

instance Functor Maybe where
  -- Use Maybe monad
  fmap f ma = do
    a <- ma
    return $ f a

-- Examples:
--
-- fmap (+1) (Just 3)


