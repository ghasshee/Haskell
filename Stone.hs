module Stone where 


-- show idempotent elements in  Z/nZ  
idemp n = [ x | x <- [0 .. n-1] , mod (x^2) n == x] 


-- decompose n into primes 
primes n = [ p | p <- [2 .. div n 2], mod n p == 0, [] == [ k | k <- [2 .. p-1], mod p k == 0] ] 


-- compute radical 
rad n = foldr (*) 1 (primes n) 


-- display idemp n by radicals  
idemp_rad n = map rad $ idemp n 


-- display idemp n by each primes
idemp_primes n = map primes $ idemp n 


-- exclude primes which is not included in primes n 
idemp_struct n = map ( filter (\x -> elem x (primes n)) . primes ) $ idemp n 


-- structure of pierce decomposition
structure n = zip (idemp n) (idemp_struct n)

-- show it! 
show_structure n = do print ("Z_"++show n++show(primes n)); mapM print $ structure n 


-- e.g. 
z30     = show_structure 30
z70     = show_structure 70
z210    = show_structure 210 
-- z210    =  show_structure (2*3*5*7)
z2310   = show_structure (2*3*5*7*11)

z1024   = show_structure 1024

structures n        = map (\x -> ("Z_"++show x++show(primes x),structure x)) [0..n] 

show_structures n   = mapM print $ structures n 

fields n            =  filter (\x -> length (snd x) == 2) $ structures n 

show_fields n       = mapM print $ fields n 

rings n             = filter (\x -> length (snd x) > 2) $ structures n 

show_rings n = mapM print $ rings n 




mulmod p n m = n*m `mod` p

ring_table p = [ [ mulmod p n m | n <- [0..p-1] ]  | m <- [0..p-1] ] 
print_ring_table p = loop $ ring_table p 
    where 
        loop [] = return () 
        loop (x:xs) = do print ("[" ++ showlist x ++ "]"); loop xs 
        showlist []  = ""
        showlist (n:ns) = 
            let delta = length (show p) - length (show n) in 
            if ns == [] 
                then replicate delta ' ' ++ show n 
                else replicate delta ' ' ++ show n ++ "," ++ showlist ns 

ring_mod_table p q = [ [ mulmod q n m | m <- [0..p-1] ] | n <- [0..p-1] ]
print_mod_table n p = loop $ ring_mod_table n p
    where 
        loop [] = return () 
        loop (x:xs) = do print ("[" ++ showlist x ++ "]"); loop xs 
        showlist []  = ""
        showlist (n:ns) = 
            let delta = length (show p) - length (show n) in 
            if ns == [] 
                then replicate delta ' ' ++ show n 
                else replicate delta ' ' ++ show n ++ "," ++ showlist ns 


ring_mul_group_table p = [ [ mulmod p n (n ^ m) | m <- [0..p-1] ] | n <- [0..p-1] ] 
print_mul_group p = loop $ ring_mul_group_table p 
    where 
        loop [] = return () 
        loop (x:xs) = do print ("[" ++ showlist x ++ "]"); loop xs 
        showlist []  = ""
        showlist (n:ns) = 
            let delta = length (show p) - length (show n) in 
            if ns == [] 
                then replicate delta ' ' ++ show n 
                else replicate delta ' ' ++ show n ++ "," ++ showlist ns 



