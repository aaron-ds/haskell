> module Images where
> import Data.Complex
> import Bitmaps
> import Control.Monad (liftM2)

> type CF = Complex Float

> point :: CF
> point = (-1.0) :+ 0.5

> type Image c = CF -> c

> cols :: Image Bool
> cols = even . floor . realPart


QUESTION 1

> grid :: Int -> Int -> CF -> CF -> Grid CF
> grid m n p q = [ [ f i j | i <- [0..m-1] ] | j <- [n-1,n-2..0] ]
>   where
>     f i j = p + (fromIntegral i * hstep :+ fromIntegral j * vstep)
>       where
>         r = q-p
>         hstep = realPart r / fromIntegral (m-1)
>         vstep = imagPart r / fromIntegral (n-1)


QUESTION 2

> sample :: Grid CF -> Image c -> Grid c
> sample g i = map (map i) g


QUESTION 3

> rows :: Image Bool
> rows = even . floor . imagPart


QUESTION 4

> chequer :: Image Bool
> chequer z = (rows z == cols z)

> merge :: (a->b->c) -> Image a -> Image b -> Image c
> merge f p q z = f (p z) (q z)

> chequer' :: Image Bool
> chequer' = merge (&&) rows cols

> chequer'' :: Image Bool
> chequer'' = liftM2 (&&) rows cols


QUESTION 5

> rings :: Image Bool
> rings = even . floor . magnitude



QUESTION 6

> wedges :: Int -> Image Bool
> wedges n z = even (floor (fromIntegral n * phase z / pi))

