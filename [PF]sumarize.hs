sumarizarer f i [] = i
sumarizarer f i (h:t) = f h (sumarizarer f i t)

sumlista = sumarizarer (+) 0
prodlista = sumarizarer (*) 1
lenlista = sumarizarer (\x y -> y + 1) 0