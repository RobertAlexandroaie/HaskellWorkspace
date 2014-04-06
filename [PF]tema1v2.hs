f1a xs = ((foldr (+) 0 . filter (\x -> x `mod` 2 == 1)) xs) `mod` 7

f1b [] = []
f1b xs = map (\(h:t) -> [h]) (filter (\x -> x>="A" && x<="Z") xs)

f1c = foldr (*) 1 . filter (\x -> x `mod` 2 == 1 )

f2 = foldl (\t h -> h:t) []

f3 :: (a->a->a)->(a->a->a)
f3 (f x y) = f y x

gcd2 0 0 = 0
gcd2 a 0 = a
gcd2 0 a = a
gcd2 a b = gcd2 b (a `mod` b)

gcd1 0 0 = 0
gcd1 a 0 = a
gcd1 0 a = a
gcd1 a b = 	if (a==b) 
			then a 
			else 	if (a>b) 
					then gcd1 (a-b) b 
					else gcd1 a (b-a)

f4 xs = foldl (\x y -> gcd2 x y) 0 xs

f5a xs = foldl (\x y -> 10*x + y) 0 xs

f5b b xs = foldr (\x y -> x + b*y) 0 xs