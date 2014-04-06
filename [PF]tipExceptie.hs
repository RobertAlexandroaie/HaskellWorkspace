data MaybeInt = MyError | MyInt Int deriving Show

myDiv::Int->Int->MaybeInt
myDiv x 0 = MyError
myDiv x y = MyInt (div x y)

f::MaybeInt->MaybeInt->MaybeInt
f MyError MyError = MyError
f MyError (MyInt x) = MyError
f (MyInt x) MyError = MyError
f (MyInt x) (MyInt y) = myDiv x y

-----------------------------------------------------------------------
data Maybe' a = Nimic | Ceva a deriving Show
--sau eroare sau tip a

fct1::String -> (Maybe' String)
fct1 "" = Nimic
fct1 (h:t) = Ceva t

fct2::(Maybe' String)->(Maybe' String)
fct2 Nimic = Nimic
fct2 (Ceva xs) = fct1 xs


------------------------------------------------------------------------
--data Maybe a = Nothing | Just a <- predefinit
------------------------------------------------------------------------

--functie rezolva care rezolva ecuatia de ordin I: intrare: a,b . rezolvam ax+b=0
rezolva::Double->Double-> (Maybe Double)
rezolva 0 x = Nothing
rezolva a b = (Just (-b/a))


