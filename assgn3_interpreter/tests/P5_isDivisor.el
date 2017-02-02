(LET 
	isDivisor  = (L x . (L d . (Eq (Mult (Div x d) d) x)))
 IN 
    ((isDivisor 12) 5))