filtrare f [] = []
filtrare f (h:t) = if f h then h:(filtrare f t) else filtrare f t