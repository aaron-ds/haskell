FPR PRACTICAL 5 - trees

> import Data.List (sort)

> data Tree a = Empty
>             | Node (Tree a) a (Tree a)
>   deriving Show

> t1 :: Tree Integer
> t1 = Empty

singleton makes a singleton tree:

> singleton :: Integer -> Tree Integer
> singleton n = Node Empty n Empty

> t3 :: Tree Integer
> t3 = Node (singleton 1) 2 (singleton 3)


QUESTION 1

> size :: Tree a -> Integer
> size Empty = 0
> size (Node t a u) = size t + 1 + size u


QUESTION 2

> tree :: [a] -> Tree a
> tree [] = Empty
> tree (x:xs) = Node Empty x (tree xs)

This produces a lopsided tree. More balanced?

> tree' :: [a] -> Tree a
> tree' [] = Empty
> tree' xs = Node (tree' ys) z (tree' zs)
>   where
>     l = length xs `div` 2
>     (ys, z:zs) = splitAt l xs


QUESTION 3

> memberT :: Eq a => a -> Tree a -> Bool
> memberT _ Empty = False
> memberT x (Node t y u) = (x==y) || memberT x t || memberT x u


QUESTION 4

> searchTree :: Ord a => [a] -> Tree a
> searchTree [] = Empty
> searchTree (x:xs) = insert x (searchTree xs)
>   where
>     insert x (Node t y u)
>       | x<=y = Node (insert x t) y u
>       | x>y = Node t y (insert x u)
>     insert x Empty = Node Empty x Empty


QUESTION 5

> memberS :: Ord a => a -> Tree a -> Bool
> memberS _ Empty = False
> memberS x (Node t y u)
>   | x==y = True
>   | x<y = memberS x t
>   | otherwise = memberS x u

