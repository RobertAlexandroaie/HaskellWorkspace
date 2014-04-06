f1 xs = foldr (\x _ -> x + 1) 0 xs

f = (foldr (+) 0).(filter (\x -> x `mod` 2 == 0))
f2 x = (f x) `mod` 10 

f3 xs = foldl (\x y -> 10*x + y) 0 xs

f4 z b = foldr (\x y -> x + y*b) 0 z

f5 xs = foldl (\t h -> h:t) [] xs