> module Bitmaps where

> import Data.List (genericLength)

> type Grid a = [[a]]

> catPic :: Grid Char
> catPic = 
>   [ "  *     *  ",
>     " * *   * * ",
>     " *  ***  * ",
>     "*         *",
>     "*  *   *  *",
>     "*    *    *",
>     " *       * ",
>     "  ******   " ]


QUESTION 1

> bwCharRender :: Grid Char -> IO ()
> bwCharRender = putStr . unlines


QUESTION - shapes

> solidSquare :: Int -> Grid Char
> solidSquare n
>   = [ [ '*' | _ <- [1..n] ] | _ <- [1..n] ]

An alternative approach is as follows:

  replicate n (replicate n '*')

> hollowSquare :: Int -> Grid Char
> hollowSquare n
>   = [ row y | y <- [1..n] ]
>     where
>       row y = if y==1 || y==n then replicate n '*'
>               else "*" ++ replicate (n-2) ' ' ++ "*"

> hollowSquare' :: Int -> Grid Char
> hollowSquare' n
>   = [ [ ch x y | x <- [1..n] ] | y <- [1..n] ]
>     where
>       ch x y = if x==1 || y==1 || x==n || y==n then '*' else ' '

> solidRect :: Int -> Int -> Grid Char
> solidRect m n
>   = [ [ '*' | _ <- [1..n] ] | _ <- [1..m] ]

> hollowSquare'' :: Int -> Grid Char
> hollowSquare'' 1 = ["*"]
> hollowSquare'' n = solidRect 1 n ++ middleBit ++ solidRect 1 n
>   where
>     middleBit = replicate (n-2) ("*" ++ replicate (n-2) ' ' ++ "*")

> rightTriangle :: Int -> Grid Char
> rightTriangle n = [ row n y | y <- [1..n] ]
>   where
>     row n y = replicate (n-y) ' ' ++ replicate y '*'

> rightTriangle' :: Int -> Grid Char
> rightTriangle' n = [ replicate (n-y) ' ' ++ replicate y '*'
>                    | y <- [1..n] ]

> rightTriangle'' :: Int -> Grid Char
> rightTriangle'' 1 = ["*"]
> rightTriangle'' n = extend (rightTriangle'' (n-1))
>   where
>     extend g = map (' ':) g ++ [replicate n '*']


QUESTION 3

> bwCharView :: Grid Bool -> Grid Char
> bwCharView = map (map (\ b -> if b then '*' else ' '))

> bwCharView' :: Grid Bool -> Grid Char
> bwCharView' bss = [ [ if b then '*' else ' ' | b <- bs ]
>                   | bs <- bss ]



> catBitmap :: Grid Bool
> catBitmap = [ 
>     [False,False,True,False,False,False,False,False,True,False,False],
>     [False,True,False,True,False,False,False,True,False,True,False],
>     [False,True,False,False,True,True,True,False,False,True,False],
>     [True,False,False,False,False,False,False,False,False,False,True],
>     [True,False,False,True,False,False,False,True,False,False,True],
>     [True,False,False,False,False,True,False,False,False,False,True],
>     [False,True,False,False,False,False,False,False,False,True,False],
>     [False,False,True,True,True,True,True,True,False,False,False]
>   ]

> fprBitmap :: Grid Bool
> fprBitmap = [
>     [ f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f ],
>     [ f, t, t, t, t, f, f, t, t, t, f, f, f, t, t, t, f, f ],
>     [ f, t, f, f, f, f, f, t, f, f, t, f, f, t, f, f, t, f ],
>     [ f, t, t, t, f, f, f, t, t, t, f, f, f, t, t, t, f, f ],
>     [ f, t, f, f, f, f, f, t, f, f, f, f, f, t, f, f, t, f ],
>     [ f, t, f, f, f, f, f, t, f, f, f, f, f, t, f, f, t, f ],
>     [ f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f ] ]
>   where f = False ; t = True

> type Point = (Integer,Integer)

> catPoints :: [Point]
> catPoints = 
>   [(2,0),(8,0),(1,1),(3,1),(7,1),(9,1),(1,2),(4,2),(5,2),(6,2),
>    (9,2),(0,3),(10,3),(0,4),(3,4),(7,4),(10,4),(0,5),(5,5),
>    (10,5),(1,6),(9,6),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7)]


QUESTION 4

> pointsBitmap :: [Point] -> Grid Bool
> pointsBitmap zs = [ [ (x,y) `elem` zs | x <- [0..w-1]] | y <- [0..h-1] ]
>   where
>     (xs,ys) = unzip zs
>     w = range xs
>     h = range ys
>     range xs = 1 + maximum xs - minimum xs


QUESTION 5

> gridPoints :: Grid Bool -> [Point]
> gridPoints g = concat [ [ (x,y) | (b,x) <- zip row [0..], b ]
>                       | (row,y) <- zip g [0..] ]



> logoShades :: Grid Float
> logoShades = [
>     [h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,h,1,1,1,h,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,0,h,h,h,h,0,0,h,1,1,1,h,0,0,h,h,h,h,h,h,h,h,h,h],
>     [0,0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,h,h,h,h,h,h,h,h,h],
>     [0,0,0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,0,0,h,h,h,h,0,0,1,1,1,1,1,h,0,0,h,h,h,h,h,h,h,h],
>     [0,0,0,h,h,h,h,0,0,h,1,1,1,1,1,1,0,0,0,h,h,h,h,h,h,h],
>     [0,0,0,h,h,h,h,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0],
>     [0,0,h,h,h,h,0,0,1,1,1,1,0,h,1,1,1,h,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,h,1,1,1,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0],
>     [h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,h,1,1,1,h,0,0,0,0,0,0]
>   ] where h = 0.5

> charPalette, charPaletteBlocks :: [Char]
> charPalette = " .:oO8@"
> charPaletteBlocks = " \9617\9618\9619\9608"


QUESTION 6

> greyCharView :: [Char] -> Grid Float -> Grid Char
> greyCharView p xss = map (map (look p)) xss
>   where
>     look :: [Char] -> Float -> Char
>     look p 1.0 = last p
>     look p x = p !! i where i = floor (x * genericLength p)

> fprGreymap :: Grid Float
> fprGreymap = [
>   [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ],
>   [ 0,a,a,a,a,0,0,b,b,b,0,0,0,1,1,1,0,0 ],
>   [ 0,a,0,0,0,0,0,b,0,0,b,0,0,1,0,0,1,0 ],
>   [ 0,a,a,a,0,0,0,b,b,b,0,0,0,1,1,1,0,0 ],
>   [ 0,a,0,0,0,0,0,b,0,0,0,0,0,1,0,0,1,0 ],
>   [ 0,a,0,0,0,0,0,b,0,0,0,0,0,1,0,0,1,0 ],
>   [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ] ]
>   where a = 1/3 ; b = 2/3

> data RGB = RGB Int Int Int
> 
> instance Show RGB where
>   show (RGB r g b) = show r ++" "++ show g ++" "++ show b

> fprPixmap :: Grid RGB
> fprPixmap = [
>   [ b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b ],
>   [ b,r,r,r,r,b,b,o,o,o,b,b,b,y,y,y,b,b ],
>   [ b,r,b,b,b,b,b,o,b,b,o,b,b,y,b,b,y,b ],
>   [ b,r,r,r,b,b,b,o,o,o,b,b,b,y,y,y,b,b ],
>   [ b,r,b,b,b,b,b,o,b,b,b,b,b,y,b,b,y,b ],
>   [ b,r,b,b,b,b,b,o,b,b,b,b,b,y,b,b,y,b ],
>   [ b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b ] ]
>   where r = RGB 7 0 0 ; o = RGB 7 3 0 ; y = RGB 7 7 0 ; b = RGB 0 0 0
