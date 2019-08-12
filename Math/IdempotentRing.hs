



z k             = [0 .. k-1] 
idem  k         = [ x | x <- z k, x*x `mod` k == x ] 
idemList k      = [ idem x | x<- [ 1..k ] ]
numList k       = [ x      | x<- [ 1..k ] ]
idemLen k       = length . idem $ k 
idemLenList k   = [ idemLen x | x <- [ 1 .. k ] ] 
idems k         = zip (numList k) (idemList k)
idemLens k      = zip (numList k) (idemLenList k)
maxidems        :: Integral a => [(a,Int)] -> [(a,Int)]  
maxidems        = foldr comp []  
    where   
            comp  :: Integral a => (a,Int) -> [(a,Int)] -> [(a,Int)]
            comp x [] = [x]
            comp x xs = if snd x > snd (head xs) 
                                then [x]
                                else if snd x == snd (head xs) 
                                    then x:xs 
                                    else xs




