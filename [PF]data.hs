data Tree =	Leaf Double
			| Op Char Tree Tree


height (Leaf _) = 1
height (Op _ l r) = 1 + (max (height l) (height r))



countLeafs (Leaf _) = 1
countLeafs (Op _ l r) = countLeafs l + countLeafs r

countNodes (Leaf _) = 1
countNodes (Op _ l r) = 1 + countNodes l + countNodes r

countInternalNodes (Op x l r) = (countNodes (Op x l r)) - (countLeafs (Op x l r)) - 1

maxPath (Leaf _) = 1
maxPath (Op _ l r) = max (maxPath l) (max (maxPath r) (1 + (height l) + (height r))) 