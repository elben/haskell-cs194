{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM

------------------
-- Exercise 1
------------------

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

------------------
-- Exercise 2
------------------

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
              Nothing   -> Nothing
              Just expr -> Just (eval expr)

------------------
-- Exercise 3
------------------

-- Type class.
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y

-- Need to enforce the type var 'a' into ExprT. This is just an id function,
-- which enforces the return type to ExprT.
--
-- Instead of using reify, we could also do:
--
-- (mul (add (lit 2) (lit 4)) (lit 5)) :: ExprT
--
-- Usage: reify $ mul (add (lit 2) (lit 4)) (lit 5)
reify :: ExprT -> ExprT
reify = id

------------------
-- Exercise 4
------------------

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = x > 0
  add x y = x || y
  mul x y = x && y
  
newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- To test, run these expressions, which enforce the unbounded type `a` into
-- Maybe Integer, etc:
--
-- testExp :: Maybe Integer
-- testExp :: Maybe Bool
-- testExp :: Maybe MinMax
-- testExp :: Maybe Mod7


------------------
-- Exercise 5
------------------

-- Goal:
--
-- (add (lit 4) (lit 5)) returns a Program
--
-- stackVM (add (lit 4) (lit 5)) returns Right [IVal exp] (a list of integer
--   values)
--

instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  -- x and y are Programs that need to run. The result of running x and y will
  -- be the top two things in the stack.
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

-- Tests:
--
-- (mul (add (lit 3) (lit 5)) (lit 5)) :: StackVM.Program
--
-- StackVM.stackVM (mul (add (lit 3) (lit 5)) (lit 5))

compile :: String -> Maybe StackVM.Program
compile s = (parseExp lit add mul s) :: Maybe StackVM.Program

-- Tests:
--
-- compile "(3 * 4) + 9"

run :: String -> Either String StackVM.StackVal
run s = case compile s of
          Nothing -> Left "nothing"
          Just p  -> StackVM.stackVM p

-- Tests:
--
-- run "(3 * 4) + 9"

------------------
-- Exercise 6
------------------


