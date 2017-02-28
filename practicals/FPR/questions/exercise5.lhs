> import Data.List

> data Tree a = Empty | Node (Tree a) a (Tree a)
>   deriving Show

1. a function to calculate the number of elements in a tree

> size :: Tree a -> Integer
> size Empty = 0
> size (Node l v r) = 1 + size l +  size r 
>
> t :: Tree Int
> t = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)

2. a function to convert a list into a tree - this one creates a lop sided tree with all the elements
   in the right hand side

> tree :: [a] -> Tree a
> tree [] = Empty
> tree (xs) = Node (tree l) x (tree r)
>   where (l, x:r) = splitAt (length xs `div` 2) xs

3. a function that determines whether a given tree contains a specified element

> memberT :: Eq a => a -> Tree a -> Bool
> memberT _ Empty = False
> memberT x (Node l d r) = if x == d then True else memberT x l || memberT x r 

4. a function that converts a list into a search tree

> searchTree :: Ord a => [a] -> Tree a
> searchTree = tree . sort

> searchTree' :: Ord a => [a] -> Tree a
> searchTree' [] = Empty
> searchTree' (x:xs) = 
