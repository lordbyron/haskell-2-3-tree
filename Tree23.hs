module Tree23 (
  Tree(..),
  insert,
  contains,
  mkTree
) where

import qualified Data.List

data Leaf a = One a | Two a a
  deriving (Eq)
data Node a = Bi (Tree a) a (Tree a) | Tri (Tree a) a (Tree a) a (Tree a)
  deriving (Eq)
data Tree a = Empty | Leaf (Leaf a) | Node (Node a) | Promote (Node a)
  deriving (Eq)

mkTree :: (Ord a) => [a] -> Tree a
mkTree = foldl (\t x -> insert t x) Empty

contains :: (Ord a) => Tree a -> a -> Bool
contains Empty x              = False
contains (Leaf (One v)) x     = x == v
contains (Leaf (Two v1 v2)) x = x == v1 || x == v2
contains (Node (Bi t1 v t2)) x
  | x == v = True
  | x  < v = contains t1 x
  | x  > v = contains t2 x
contains (Node (Tri t1 v1 t2 v2 t3)) x
  | x == v1   = True
  | x == v2   = True
  | x  < v1   = contains t1 x
  | x  > v2   = contains t3 x
  | otherwise = contains t2 x

-- strip leading Promote
reduce :: (Ord a) => Tree a -> Tree a
reduce (Promote p) = Node p
reduce t           = t

-- should not be called with two 1-leaves, or a tri-node and any leaf
reduce2 :: (Ord a) => Tree a -> a -> Tree a -> Tree a
reduce2 (Leaf l1) v (Leaf l2)               = Node (Bi (Leaf l1) v (Leaf l2))
reduce2 (Node n1) v (Node n2)               = Node (Bi (Node n1) v (Node n2))
reduce2 (Promote (Bi t1 v1 t2)) v2 t3       = Node (Tri t1 v1 t2 v2 t3)
reduce2 t1 v1 (Promote (Bi t2 v2 t3))       = Node (Tri t1 v1 t2 v2 t3)

-- should not be called with three 1-leaves, or a tri-node and any leaves
reduce3 :: (Ord a) => Tree a -> a -> Tree a -> a -> Tree a -> Tree a
reduce3 (Leaf l1) v1 (Leaf l2) v2 (Leaf l3) = Node (Tri (Leaf l1) v1 (Leaf l2) v2 (Leaf l3))
reduce3 (Node n1) v1 (Node n2) v2 (Node n3) = Node (Tri (Node n1) v1 (Node n2) v2 (Node n3))
reduce3 t1 v1 t2 v2 (Promote (Bi t3 v3 t4)) = Promote (Bi (Node (Bi t1 v1 t2)) v2 (Node (Bi t3 v3 t4)))
reduce3 t1 v1 (Promote (Bi t2 v2 t3)) v3 t4 = Promote (Bi (Node (Bi t1 v1 t2)) v2 (Node (Bi t3 v3 t4)))
reduce3 (Promote (Bi t1 v1 t2)) v2 t3 v3 t4 = Promote (Bi (Node (Bi t1 v1 t2)) v2 (Node (Bi t3 v3 t4)))

insert :: (Ord a) => Tree a -> a -> Tree a
insert t x = reduce (ins t x)

ins :: (Ord a) => Tree a -> a -> Tree a
ins Empty x = Leaf (One x)
ins (Leaf (One v)) x
  | x == v    = Leaf (One v)
  | x  < v    = Leaf (Two x v)
  | x  > v    = Leaf (Two v x)
ins (Leaf (Two v1 v2)) x
  | x == v1   = Leaf (Two v1 v2)
  | x == v2   = Leaf (Two v1 v2)
  | x  < v1   = Promote (Bi (Leaf (One x)) v1 (Leaf (One v2)))
  | x  > v2   = Promote (Bi (Leaf (One v1)) v2 (Leaf (One x)))
  | otherwise = Promote (Bi (Leaf (One v1)) x (Leaf (One v2)))
ins (Node (Bi t1 v t2)) x
  | x == v    = Node (Bi t1 v t2)
  | x  < v    = reduce2 (ins t1 x) v t2
  | x  > v    = reduce2 t1 v (ins t2 x)
ins (Node (Tri t1 v1 t2 v2 t3)) x
  | x == v1   = Node (Tri t1 v1 t2 v2 t3)
  | x == v2   = Node (Tri t1 v1 t2 v2 t3)
  | x  < v1   = reduce3 (ins t1 x) v1 t2 v2 t3
  | x  > v2   = reduce3 t1 v1 t2 v2 (ins t3 x)
  | otherwise = reduce3 t1 v1 (ins t2 x) v2 t3

{- FORMATTING -}

instance Show a => Show (Node a) where
  show n = showNode n 0

showNode :: (Show a) => Node a -> Int -> String
showNode (Bi t1 v t2)         i = (showTree t1 (i+1)) ++ "\n" ++ (replicate (i*2) ' ') ++ (show v) ++ "\n" ++ (showTree t2 (i+1))
showNode (Tri t1 v1 t2 v2 t3) i = (showTree t1 (i+1)) ++ "\n" ++ (replicate (i*2) ' ') ++ (show v1) ++ "\n" ++ (showTree t2 (i+1)) ++ "\n" ++ (replicate (i*2) ' ') ++ (show v2) ++ "\n" ++ (showTree t3 (i+1))

instance Show a => Show (Leaf a) where
  show l = showLeaf l 0

showLeaf :: (Show a) => Leaf a -> Int -> String
showLeaf (One v)     i = (replicate (i*2) ' ') ++ show v
showLeaf (Two v1 v2) i = (replicate (i*2) ' ') ++ (show v1) ++ "\n" ++ (replicate (i*2) ' ') ++ (show v2)

instance Show a => Show (Tree a) where
  show t = showTree t 0

showTree :: (Show a) => Tree a -> Int -> String
showTree Empty _    = "[]"
showTree (Leaf l) i = showLeaf l i
showTree (Node n) i = showNode n i
