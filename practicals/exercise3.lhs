exercise 3 - recursive definitions on lists

1. a function that calculates the product of a list of integers

> prod :: [Int] -> Int
> prod [] = 1
> prod (x:xs) = x * prod xs

2. a function that determines whether every element of a list of booleans is true

> allTrue :: [Bool] -> Bool
> allTrue [] = True
> allTrue (x:xs) = if x == False then False else allTrue xs

3. a funtion that determines whether every element of a list of booleans is false

same as 2

4. a function that decrements each integer element of a list by one

> decAll :: [Int] -> [Int]
> decAll [] = []
> decAll (x:xs) = (x - 1) : decAll xs

5. a function that converts zeros to false and any other numbers to true

> convertIntBool :: [Int] -> [Bool]
> convertIntBool [] = []
> convertIntBool (x:xs) = (if x == 0 then False else True) : convertIntBool xs

6. zip function

> pairUp :: [Int] -> [Char] -> [(Int,Char)]
> pairUp [] _ = []
> pairUp _ [] = []
> pairUp (x:xs) (y:ys) = (x,y) : pairUp xs ys

7. a function that returns the prefix of the specified length of the given list

> takePrefix :: Int -> [a] -> [a]
> takePrefix 0 _ = []
> takePrefix n (x:xs) = x : takePrefix (n-1) xs

8. a function that drops the prefixes

> dropPrefix :: Int -> [a] -> [a]
> dropPrefix 0 xs = xs
> dropPrefix n (x:xs) = dropPrefix (n-1) xs

9. a function that determines whether a list contains a specified element

> member :: Eq a => [a] -> a -> Bool
> member [] _ = False
> member (x:xs) y = if y == x then True else member xs y

10. a function that determines whether two lists contain the same elements in the same order

> equals :: Eq a => [a] -> [a] -> Bool
> equals [] [] = True
> equals [] _ = False
> equals _ [] = False
> equals (x:xs) (y:ys) = if x /= y then False else equals xs ys
