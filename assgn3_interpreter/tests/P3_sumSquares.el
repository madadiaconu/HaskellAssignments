(LET 
	square = (L x . (Mult x x)) 
IN (LET 
       sumSquares = (L a . (L b . (Add (square a) (square b))))
       IN 
          ((sumSquares 3) 4)))