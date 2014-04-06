double = \x -> 2*x
triple = \x -> 3*x

parcurge f [] = []
parcurge f (h:t) = (f h) : (parcurge f t)