insert1 x [] = [x]
insert1 x xs = 	if (x > last xs)
				then xs ++ [x]
				else	if ((x <= last xs) && (x > head xs))
						then [head xs] ++ insert1 x (tail xs)
						else [x]++xs

sort1 [] = []
sort1 (h:t) = insert1 h (sort1 t)

isDecreasing [] = True
isDecreasing xs = and (map (\(x,y) -> x >= y) (zip xs (tail xs)))

suffixDecreasing [x] = [x]
suffixDecreasing xs = 	if (isDecreasing xs) 
						then xs 
						else suffixDecreasing (tail xs)
						
whereDecreasing [x] = x
whereDecreasing (h:t) = 	if (isDecreasing t)
							then h
							else whereDecreasing t
							
prefixDecreasing xs = reverse (drop (length (suffixDecreasing xs) +1) (reverse xs))

smallest [x] = x
smallest (h:t) = 	if (h <= smallest t)
					then h
					else smallest t


smallestGreaterThan [y] x = if (y > x) then y else x
smallestGreaterThan xs x =	if ((smallest xs) > x) 
							then (smallest xs)
							else smallestGreaterThan (init xs) x

remove [] x = []
remove [y] x = if (y == x) then [] else [y]
remove (h:t) x = 	if (h == x) then t
					else [h] ++ (remove t x)

nextPermutation xs = (prefixDecreasing xs) ++ [smallestGreaterThan (suffixDecreasing xs) (whereDecreasing xs)] ++  (sort1 (insert1 (whereDecreasing xs ) (remove (suffixDecreasing xs) (smallestGreaterThan (suffixDecreasing xs) (whereDecreasing xs)))))
isLastPermutation xs = isDecreasing xs

permutations1 xs = 	if (isLastPermutation xs)
					then [xs]
					else [xs]++(permutations1 (nextPermutation xs))

list 1 = [1]
list xs = [xs] ++ (list (xs-1))
permutations x = permutations1 (sort1 (list x))