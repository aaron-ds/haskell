> module Images where
> import Data.Complex
> import Bitmaps

> type CF = Complex Float

> point :: CF
> point = (-1.0) :+ 0.5

> type Image c = CF -> c

cols returns true if the real part of the CF is even, otherwise returns false

> cols :: Image Bool
> cols = even . floor . realPart

 > grid :: Int -> Int -> CF -> CF -> Grid CF
 > grid m n p q = [ [ f i j | x <- [0..m-1] | y <- [0..n-1]]
 >   where 

 > grid :: Int -> Int -> CF -> CF -> Grid CF
 > grid c r (pr :+ pi) (qr :+ qi) = [[ zr :+ zi | zr <- for c pr qr ] | zi <- for r qi pi ]
 >                                  where for n a b = take n [a, a + d.. ]
 >                                        where d = (b - a) / fromIntegral (n - 1)

> grid :: Int -> Int -> CF -> CF -> Grid CF
> grid c r (pr :+ pi) (qr :+ qi)
>   = [[ zr :+ zi | zr <- for c pr qr ] | zi <- for r qi pi ]
>     where
>       for n a b = take n [ a, a+d..]
>         where d = (b-a) / fromIntegral (n-1)

2.

> sample :: Grid CF -> Image c -> Grid c
> sample points image = map (map image) points

> rows :: Image Bool
> rows = even . floor . imagPart

> chequer :: Image Bool
> chequer i = even ((floor (realPart i)) + (floor (imagPart i)))

> rings :: Image Bool
> rings = even . floor . magnitude

 > wedges :: Int -> Image Bool

