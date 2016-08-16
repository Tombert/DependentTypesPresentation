module Printf 

data Format = FInt Format  
            | FString Format
            | FOther Char Format
            | FEnd

format : List Char -> Format
format ('%'::'s'::es) = FString (format es)
format ('%'::'i'::es) = FInt (format es)
format (x::xs) = FOther x (format xs)
format [] = FEnd

formatString : String -> Format
formatString = format . unpack 


interpFormat : Format -> Type
interpFormat (FInt x) = Int -> interpFormat x
interpFormat (FString x) = String -> interpFormat x
interpFormat (FOther _ x) = interpFormat x
interpFormat FEnd = String

printf "Hello %d you goober, I am %i years old" -- String - Int -> String

toFunction : String -> (fmt : Format) -> interpFormat fmt
toFunction acc (FInt x) = \i => toFunction (acc ++ (show i)) x
toFunction acc (FString x) = \i => toFunction (acc ++ i) x
toFunction acc (FOther x y) = toFunction (acc ++ (singleton x)) y
toFunction acc FEnd = acc


printf : (s : String) -> interpFormat (formatString s)
printf s = toFunction "" (formatString s)


