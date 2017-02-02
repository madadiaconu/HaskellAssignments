(LET 
	isDivisor  = (L a . (L d . (Eq (Mult (Div a d) d) a)))
 IN 
    (LET 
        findDivisorRec = (L y . (L i . (IF (Eq y i) 
                                           False 
                                           (IF ((isDivisor y) i) True
                                               ((findDivisorRec y) (Add i 1))))))
        IN 
           (LET 
               isPrime = (L x . (Not ((findDivisorRec x) 2)))
            IN 
               (isPrime 32))))
 