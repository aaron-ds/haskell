FPR Assignment, February 2017 - Futoshiki puzzles

----------------------------------------------------------------------

We will use the following imports:

> import Data.Ord
> import Data.Char
> import Data.Maybe
> import Data.Either
> import Data.List

----------------------------------------------------------------------

Here are the datatype definitions from the assignment, specifying
problems, working states, and solutions:

> type Candidate = Char

> size :: Int 
> size = 5

> candidates :: [Candidate]
> candidates = take size ['A'..]

> type Grid a = [[a]]

> data Direction = H | V deriving (Eq,Show)
> type Constraint = (Direction, Ordering, Int, Int)
> type Constraints = [Constraint]

> type Problem = (Grid (Maybe Candidate), Constraints)

> type Choices = [Candidate]
> type Working = (Grid Choices, Constraints)
> type Solution = (Grid Candidate, Constraints)

----------------------------------------------------------------------

3. Displaying puzzles
could a foldr be used here??
concat map could be replaced by flatmap
could possibly use unlines instead of the first concat
- use composition here?


> showProblemBoxes :: Problem -> String
> showProblemBoxes p = concat (map showRow g) 
>   where g = fst p
>
> showRow :: [(Maybe Candidate)] -> String
> showRow r = (concat (map showBox r)) ++ "\n"
>   where showBox Nothing = "[ ]"
>         showBox (Just c) = "[" ++ [c] ++ "]"


2. could do something recursive where we keep building
have another look at this, the recursive way that takes an accumulator might be less error prone
could check whether i is bigger than the length of the list, if so just return the list - do this with guard clauses?

> at :: Int -> (a -> a) -> [a] -> [a]
> at i f xs | i >= length xs = xs
>           | otherwise = (take i xs) ++ [f (xs !! i)] ++ (drop (i + 1) xs)






Finally, here are the three futoshiki puzzles discussed in the assignment:

> easy537 :: Problem
> easy537 = (
>   [  [ Nothing, Just 'B', Nothing, Nothing, Nothing ],
>      [ Nothing, Nothing, Nothing, Nothing, Nothing ],
>      [ Nothing, Nothing, Nothing, Nothing, Nothing ],
>      [ Nothing, Just 'C', Nothing, Just 'B', Nothing ],
>      [ Nothing, Nothing, Nothing, Nothing, Nothing ] ],
>   [  (H,GT,0,0), (H,GT,1,2), (H,LT,1,3), (H,GT,2,3), (H,GT,3,0), 
>      (H,LT,3,2), (V,LT,0,0), (V,GT,1,1), (V,GT,1,2), (V,GT,2,1), 
>      (V,LT,2,2), (V,GT,3,2), (V,GT,4,0) ] )

> medium538 :: Problem
> medium538 = 
>   (  replicate size (replicate size Nothing),
>      [  (H,LT,0,0), (H,GT,1,0), (H,GT,2,1), (H,LT,3,1), (H,LT,3,3),
>         (V,GT,0,3), (V,LT,1,0), (V,GT,1,2), (V,GT,1,3), (V,LT,2,0), 
>         (V,GT,2,1), (V,LT,3,0), (V,LT,3,1), (V,LT,3,3) ])

> extreme :: Problem
> extreme = (
>   [  [ Nothing, Nothing, Nothing, Nothing, Just 'D' ],
>      [ Nothing, Nothing, Nothing, Nothing, Nothing ],
>      [ Nothing, Nothing, Nothing, Nothing, Nothing ],
>      [ Nothing, Nothing, Nothing, Nothing, Nothing ],
>      [ Nothing, Nothing, Nothing, Nothing, Nothing ] ],
>   [  (H,GT,0,0), (H,LT,1,0), (H,LT,2,0), (H,GT,0,3), 
>      (H,GT,1,3), (H,LT,0,4), (H,LT,3,4), (V,LT,1,1), 
>      (V,GT,2,3), (V,LT,3,2), (V,GT,4,0) ])

----------------------------------------------------------------------
