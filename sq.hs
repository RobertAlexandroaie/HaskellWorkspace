qs :: [Ingeger]->[Integer]
qs [] = []
qs (h:t) = (qs filter (<=h) t) ++ [h] ++ (qs filter (>h) t)

insert x [] = [x]
insert x (h:t) = 	if(x<h)
					then x:h:t
					else h:x:t

sort::[Integer] -> [Integer]
sort [] = []
sort (h:t) = insert h (sort t)