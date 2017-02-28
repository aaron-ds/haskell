> module Fractals where
> import Bitmaps
> import Images

> next :: CF -> CF -> CF
> next c z = z*z + c


QUESTION 1

> mandelbrot :: CF -> [CF]
> mandelbrot c = zs where zs = 0 : map (next c) zs

Or, using iterate:

> mandelbrot' c = iterate (next c) 0

remember:

  iterate f x = x : iterate f (f x)

or

  iterate f x = zs where zs = x : map f zs

> mandelbrot'' c = evolve 0
>   where evolve z = z : evolve (next c z)



> rgbPalette :: [RGB]
> rgbPalette = 
>   [ RGB i 0 15 | i <- [15,14..0] ]  ++  --  purple to blue
>   [ RGB 0 i 15 | i <- [0..15] ]     ++  --  blue to cyan
>   [ RGB 0 15 i | i <- [15,14..0] ]  ++  --  cyan to green
>   [ RGB i 15 0 | i <- [0..15] ]     ++  --  green to yellow
>   [ RGB 15 i 0 | i <- [15,14..0] ]      --  yellow to red
