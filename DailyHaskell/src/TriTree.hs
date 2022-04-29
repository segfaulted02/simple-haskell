module TriTree where
import Text.Printf (vFmt)

data TriTree a = Empty | 
                 NodeOne a (TriTree a) (TriTree a) (TriTree a) | 
                 NodeTwo a a (TriTree a) (TriTree a) (TriTree a) 
                 deriving (Eq,Show)

--function that searches for a value in a TriTree
--accepts a value and a TriTree then returns true or false
search :: (Ord a) => a -> TriTree a -> Bool
search _ Empty = False
search x (NodeOne v left middle right) = x == v || x `search` left ||
    x `search` middle || x `search` right
search x (NodeTwo v1 v2 left middle right) = x == v1 || x == v2 ||
    x `search` left || x `search` middle || x `search` right

--function that inserts a value into a TriTree
--accepts a value and a TriTree then returns a TriTree
insert :: Ord a => a -> TriTree a -> TriTree a
insert x Empty = (NodeOne x Empty Empty Empty)
insert x (NodeOne v left middle right)
    | x < v = NodeTwo x v left middle right
    | x >= v = NodeTwo v x left middle right
insert x (NodeTwo v1 v2 left middle right)
    | x < v1 = NodeTwo v1 v2 (insert x left) middle right
    | x < v2 = NodeTwo v1 v2 left (insert x middle) right
    | x >= v2 = NodeTwo v1 v2 left middle (insert x right)

--function that repeatedly calls the insert function to add a list of elements into a TriTree
--accepts a list of elements and a TriTree then returns a TriTree
insertList :: Ord a => [a] -> TriTree a -> TriTree a
insertList lst tree = foldr (insert) tree lst

--function that checks if two TriTrees are identical
--accepts two TriTrees then returns true or false
identical :: Eq a => TriTree a -> TriTree a -> Bool
identical Empty Empty = True
identical Empty _ = False
identical _ Empty = False
identical (NodeOne x l1 m1 r1) (NodeOne y l2 m2 r2) = (x == y && l1 == l2 && 
    m1 == m2 && r1 == r2)
identical (NodeTwo x1 x2 l1 m1 r1) (NodeTwo y1 y2 l2 m2 r2) = (x1 == y1 && x2 == y2 && l1 == l2 
    && m1 == m2 && r1 == r2)

--function that applies a function to every value in a TriTree
--accepts a function and a TriTree then returns a TriTree
treeMap :: (a -> b) -> TriTree a -> TriTree b
treeMap _ Empty = Empty
treeMap func (NodeOne x left middle right) = NodeOne (func x) (treeMap func left)
    (treeMap func middle) (treeMap func right)
treeMap func (NodeTwo x y left middle right) = NodeTwo (func x) (func y) (treeMap func left)
    (treeMap func middle) (treeMap func right)

--function that "folds" a TriTree Pre Order by applying a function to the values
--accepts a function, an initial value, and a TriTree then returns a value
treeFoldPreOrder :: (a -> a -> a) -> a -> TriTree a -> a
treeFoldPreOrder func value Empty = value
treeFoldPreOrder func v (NodeOne x l m r) = (treeFoldPreOrder func (treeFoldPreOrder func 
    (treeFoldPreOrder func (func v x) l) m) r)
treeFoldPreOrder func v (NodeTwo x y l m r) = (treeFoldPreOrder func (treeFoldPreOrder func 
    (treeFoldPreOrder func (func (func v x) y) l) m) r)

--function that "folds" a TriTree In Order by applying a function to the values
--accepts a function, an initial value, and a TriTree then returns a value
treeFoldInOrder :: (a -> a -> a) -> a -> TriTree a -> a
treeFoldInOrder func value Empty = value
treeFoldInOrder func v (NodeOne x l m r) = (treeFoldInOrder func (treeFoldInOrder func 
    (func x (treeFoldInOrder func v l))m)r)
treeFoldInOrder func v (NodeTwo x y l m r) = (treeFoldInOrder func (func y (treeFoldInOrder func 
    (func x (treeFoldInOrder func v l))m))r)

--function that "folds" a TriTree Post Order by applying a function to the values
--accepts a function, an initial value, and a TriTree then returns a value
treeFoldPostOrder :: (a -> a -> a) -> a -> TriTree a -> a
treeFoldPostOrder func value Empty = value
treeFoldPostOrder func v (NodeOne x l m r) = (func x (treeFoldPostOrder func 
    (treeFoldPostOrder func (treeFoldPostOrder func v l) m) r))
treeFoldPostOrder func v (NodeTwo x y l m r) = (func y (func x (treeFoldPostOrder func 
    (treeFoldPostOrder func (treeFoldPostOrder func v l) m) r)))