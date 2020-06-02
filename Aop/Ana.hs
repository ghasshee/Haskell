module Aop.Ana where 
import Aop.Int
import Aop.List

ana p g b  | p b  = [] 
ana p g b         = a : (ana p g b')   where (a,b') = g b 



p (as, bs)          = ( as == [] ) || ( bs == [] )
g (a:as, b:bs)      = ( (a,b),(as,bs) )

zip :: (Eq a, Eq b) => ([a],[b]) -> [(a,b)]
zip = ana p g 

zip' :: (Eq a, Eq b) => [a] -> [b] -> [(a,b)]
zip' = curry ( ana p g )


iterate :: (a->a) -> a -> [a]
iterate f = ana (const False) (\a -> (a,f a)) 


unfold = ana 

isZero 0 = True
isZero _ = False
pred'' n = (n,pred n) 
hoge = unfold isZero pred'' 10 



fac = ( foldr (*) 1 ) . ( unfold isZero pred'' ) 
