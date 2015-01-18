module Week07 where

data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf a = Node Empty a Empty

treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) =  1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty = 0
treeSum (Node l a r) = a + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flatten :: Tree a => [a]
flatten Empty = []
flatten (Node l a r) = flatten l ++ [a] ++ flatten r

-- We can generalize a folding (i.e. reduce) method

-- b is the resulting type, a is the node type. Note (b -> a -> b -> b) is a
-- function that takes left-branch, current element, right-branch of the tree.
treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold b _ Empty = b
treeFold b f (Node l a r) = f (treeFold b f l) a (treeFold b f r)

treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l a r -> l + a + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l a r -> l ++ [a] ++ r)
