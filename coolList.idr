module CoolList

data CList : Nat -> Type -> Type where 
  Null : CList 0 a
  (::) : (x : a) ->  (xs : CList n a) -> CList (S n) a


coolHead : CList (S n) a -> a
coolHead (x :: _) = x

coolAppend : CList n a -> CList m a -> CList (n + m) a
coolAppend Null y = y
coolAppend (x :: xs) y = x :: (coolAppend xs y)

coolZip : CList n a -> CList n b -> CList n (a, b)
coolZip Null Null = Null
coolZip (x :: xs) (y :: z) = (x, y) :: coolZip xs z

coolMap : (a -> b) -> CList n a -> CList n b
coolMap f Null = Null
coolMap f (x :: xs) = f x :: coolMap f xs


coolZipWith : (a -> b -> c) -> CList n a -> CList n b -> CList n c
coolZipWith f Null Null = Null
coolZipWith f (x :: xs) (y :: ys) = f x y :: coolZipWith f xs ys
