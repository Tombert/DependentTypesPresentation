module CoolList

data CList : Nat -> Type -> Type where 
  Null : CList 0 a
  coolcons : (x : a) ->  (xs : CList n a) -> CList (S n) a


coolHead : CList (S n) a -> a
coolHead (x `coolcons` _) = x

coolAppend : CList n a -> CList m a -> CList (n + m) a
coolAppend Null y = y
coolAppend (x `coolcons` xs) y = x `coolcons` (coolAppend xs y)
