module Week2 where

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
  deriving Show

shoe :: Thing
shoe = Shoe

data FailableDouble = Failure
                    | OK Double
  deriving Show

ex01 :: FailableDouble
ex01 = Failure

ex02 :: FailableDouble
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d



data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax


stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = "The name field is " ++ show p ++ " and has name " ++ n

data IntList = Empty | Cons Int IntList
  deriving Show

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l

list1 :: IntList
list1 = Empty

list2 :: IntList
list2 = Cons 1 (Cons 2 (Cons 3 Empty))

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree1 :: Tree
tree1 = Leaf 'a'

tree2 :: Tree
tree2 = Node (Node (Leaf 'a') 10 (Leaf 'b')) 10 (Leaf 'c')
