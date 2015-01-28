module Week07 where

class MyMonad m where
  return :: a -> m a

  -- "bind"
  (>>=) :: m a -> (a -> m b) -> m b

  -- do nothing with the first one's computed value
  (>>) :: m a -> m b -> m b
  m1 >> m2 = m1 Week07.>>= (\_ -> m2)

instance MyMonad Maybe where
  return = Just
  (>>=) a k = case a of
              Nothing -> Nothing
              Just x  -> k x

