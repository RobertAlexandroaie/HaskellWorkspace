--f x = f (x+1)
l = 1:l
f x = x:(f (x+1))

take1 0 _ = []
take1 _ [] = []
take1 n (h:t) = h:(take1 (n-1) t)

infli x = x:(infli (x+2))
li = infli 1
lp = infli 0

nextFibo x y = x + y
inflist x y = x: inflist y (nextFibo x y) 
fiboList = inflist 1 2

nrElem x [] = 0
nrElem x (h:t) = 	if (x == h)
					then (1 + (nrElem x t))
					else 0

start = [1]
next [] = []
next (h:t) = [(nrElem h (h:t))] ++ [h] ++ (next (drop (nrElem h (h:t)) (h:t)))
listinf x = x : (listinf (next x))
sirFain = listinf start

diviz 0 x = True
diviz 1 x = False
diviz x 1 = False
diviz x y = if (x `mod` y == 0)
			then True
			else (diviz x (y-1))

prim 2 = True
prim 3 = True
prim x = 	if (diviz x (x-1))
			then False
			else True
			
infl x = 	if (prim x)
			then x : infl (x+1)
			else infl (x+1)

listaPrime = infl 2

pi_ = g(1,0,1,1,3,3) where
  g (q,r,t,k,n,l) = 
   if 4*q+r-t < n*t
    then n : g (10*q, 10*(r-n*t), t, k, div (10*(3*q+r)) t - 10*n, l)
    else g (q*k, (2*q+r)*l, t*l, k+1, div (q*(7*k+2)+r*l) (t*l), l+2)