(LET 
	xOr = (L x . (L y . (And (Or x y) (Not (And x y)))))
IN 
	((xOr False) False))