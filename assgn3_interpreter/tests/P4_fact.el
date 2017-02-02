(LET 
	fact = (L x . 
				(IF (Eq x 1) 
					1 
					(Mult x (fact (Sub x 1))))) 
IN 
	(fact 3))