-- count

count1 _ [] = 0
count1 x xs = foldr (+) 0 (keepcharx x xs)

keepcharx _ [] = []
keepcharx x (h:t) = 	if(x==h)
						then 1:keepcharx x t
						else keepcharx x t