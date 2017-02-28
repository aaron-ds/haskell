> module Fractals where
> import Bitmaps
> import Images
> import Data.Complex

> next :: CF -> CF -> CF
> next c z = z*z + c

> rgbPalette :: [RGB]
> rgbPalette = 
>   [ RGB i 0 15 | i <- [15,14..0] ]  ++  --  purple to blue
>   [ RGB 0 i 15 | i <- [0..15] ]     ++  --  blue to cyan
>   [ RGB 0 15 i | i <- [15,14..0] ]  ++  --  cyan to green
>   [ RGB i 15 0 | i <- [0..15] ]     ++  --  green to yellow
>   [ RGB 15 i 0 | i <- [15,14..0] ]      --  yellow to red

1. a function to compute the trajectory of values for a given c as an infinite list
   (next c) is the function CF -> CF
   iterate :: (a -> a) -> a -> [a]
   so (next c) 0 will generate an infinite list [a]
   we will end up with 'next c ((next c) 0)'

> mandlebrot :: CF -> [CF]
> mandlebrot c = iterate (next c) 0

no base case so an infinite list
 > mandlebrot c = 0 : map (next c) mandlebrot c // i think this is the way Andrew had defined it

2. a function to approximate whether a point has not yet diverged (the magnitude is less than 100)

> fairlyClose :: CF -> Bool
> fairlyClose c = 100 > magnitude c

3. a function to take the first 200 elements of an infinite list

> firstFew :: [CF] -> [CF]
> firstFew cs = take 200 cs

4. a function that takes a trajectory function and yields a boolean image
Image is of the type Image c = CF -> Bool
(CF -> [CF]) -> CF -> Bool
so we want, for every time we pass a point to the trajectory function 

> approximate :: (CF -> [CF]) -> Image Bool

 > approximate t c = all fairlyClose (firstFew (t c))

below, functional application binds tighter than functional composition, so all has been bound to fairly close and is then waiting for the stuff from the right to come in

> approximate t = all fairlyClose . firstFew . t 

5. a function that 
