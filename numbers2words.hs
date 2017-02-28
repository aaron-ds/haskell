import Data.Char

convert :: Int -> String
convert = convert6

units, teens, tens :: [String]
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"]

convert1 :: Int -> String
convert1 n = units !! n

digits2 :: Int -> (Int, Int)
digits2 n = (n `div` 10, n `mod` 10)

convert2 :: Int -> String
convert2 = combine2 . digits2

combine2 :: (Int, Int) -> String
combine2 (t, u)
  | t == 0 = units !! u
  | t == 1 = teens !! u
  | t >= 2 && u == 0 = tens !! (t - 2)
  | t >= 2 && u /= 0 = tens !! (t - 2) ++ "-" ++ units !! u

convert3 :: Int -> String
convert3 n
  | h == 0 = convert2 t
  | n == 0 = units !! h ++ " hundred"
  | otherwise = units !! h ++ " hundred and " ++ convert2 t
  where (h, t) = (n `div` 100, n `mod` 100)

convert6 :: Int -> String
convert6 n
  | t == 0 = convert3 h
  | h == 0 = convert3 t ++ " thousand"
  | otherwise = convert3 t ++ " thousand" ++ link h ++
                convert3 h
  where (t, h) = (n `div` 1000, n `mod` 1000)

link :: Int -> String
link h = if h < 100 then " and " else " "
