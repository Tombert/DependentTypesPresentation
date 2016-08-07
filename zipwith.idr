module ZippyPippy

import Data.Vect

zip' : Vect n a -> Vect n b -> Vect n (a, b)
zip' [] [] = []
zip' (x::xs) (y :: ys) = (x, y) :: zip' xs ys

zipWith' : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWith' _ [] [] = []
zipWith' f (x :: xs) (y :: ys) = f x y :: zipWith' f xs ys

