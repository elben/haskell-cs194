{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM
import qualified Data.Map.Strict as M

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

class HasVars a where
  var :: String -> a

data VarExprT = VarLit Integer
              | Var String
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
  deriving (Show, Eq)

instance HasVars VarExprT where
  var v = Var v

instance Expr VarExprT where
  lit x = VarLit x
  add x y = VarAdd x y
  mul x y = VarMul x y

-- Examples:
--
--  add (lit 3) (var "x") :: VarExprT

-- The HasVars and Exprs implementations below make sort of a continuation,
-- where the var mapping is threaded into the calculations. We basically form a
-- giant lambda that awaits the mapping, and once the mapping is given, the
-- functions are evoked and the values retrieved and calculated.

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var v = \mapping -> M.lookup v mapping

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \_ -> Just x
  add x y = \m -> case (x m) of
                    Nothing -> Nothing
                    Just xn -> case (y m) of
                      Nothing -> Nothing
                      Just yn -> Just (xn + yn)
  mul x y = \m -> case (x m) of
                    Nothing -> Nothing
                    Just xn -> case (y m) of
                      Nothing -> Nothing
                      Just yn -> Just (xn * yn)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs

-- Examples:
--
-- :t add (lit 3) (var "x")
-- Returns: add (lit 3) (var "x") :: (Expr a, HasVars a) => a
-- Which says, that expression is of any type a, where a implements Expr and
-- HasVars.
--
-- In the examples below, we replace the type var 'a' (above) with an explicit
-- type, namely (M.Map String Integer -> Maybe Integer). And since we've
-- implemented the type classes HasVars and Expr for (M.Map String Integer ->
-- Maybe Integer), the program can run.
--
-- withVars [("x", 6)] $ add (lit 3) (var "x")
--
-- withVars [("x", 6)] $ add (lit 3) (var "y")
--
-- withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))


