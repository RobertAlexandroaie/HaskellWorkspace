--tipuri recursive
data List a = Vid | Cons a (List a) deriving Show
-- valori posibile : Vid; Cons 1 Vid; Const 2 (Cons 1(Const 0 Vid));
mylenght (Vid) = 0;
mylenght (Cons h t)= 1 + mylenght t

--convert primeste List a si intoarce List a in sensul haskell
--convert' priesste List a haskell si intoarce List a ca mai sus
--mymap
--myfoldr
--myfoldl
--myfilter

convert (Vid) = []
convert (Cons h t) = h:convert(t)

convert' [] = Vid
convert' (h:t) = Cons (h) (convert' t)

myfilter f Vid = Vid
myfilter f (Cons h t) = if (f h) 
						then (Cons h (myfilter f t)) 
						else myfilter f t
						
mymap f Vid = Vid
mymap f (Cons h t) = Cons (f h) (mymap f t)

myinit (Cons h (Cons t Vid))= (Cons h Vid)
myinit (Cons h t) = Cons h (myinit t)

myhead Vid = Vid
myhead (Cons h t) = Cons h Vid

mytail Vid = Vid
mytail (Cons h Vid) = Vid
mytail (Cons h t) = Cons (myhead t) (mytail t)

myzip (Cons h t) Vid = Vid
myzip Vid (Cons h t) = Vid
myzip (Cons h t) (Cons h2 t2) = Cons (h,h2) (myzip t t2)

myfoldr f e Vid = e
myfoldr f e (Cons h t) = f h (myfoldr f e t)

myfoldl f e Vid = e
myfoldl f e (Cons h t) = myfoldl f (f e h) t

