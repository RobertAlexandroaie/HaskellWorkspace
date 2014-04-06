removeEmpty [] = []
removeEmpty (h:t) =	if(null h)
					then removeEmpty t
					else [h]++(removeEmpty t)
g [] = []
g (h:t) = 	if(h /= head t)
			then h:(g t)
			else g t

sd xs = and(map (\(x,y)->x>y) (zip xs (tail xs)))
