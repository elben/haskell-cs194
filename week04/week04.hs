module Week04 where

foo :: (b -> c) -> (a -> b) -> (a -> c)
-- foo f g = (\a -> (f (g a)))
foo f g = f . g
