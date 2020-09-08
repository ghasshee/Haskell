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

hylo h c p g = (foldr h c) . (unfold p g) 

fac = hylo (*) 1 isZero pred'' 


-- paramorphism 

paran h c 0 = c 
paran h c n = h (pred n) (paran h c (pred n))

fac' = paran (\n m -> (1+n)*m) 1 

parar h c []        = c 
parar h c (a:as)    = h a as (parar h c as) 



-------------------------------------------
-- PARA ~~~ CATA 
--
-- paramorphism defined as catamoprhism
------------------------------------------


data Fix f = In (f (Fix f))

cata :: Functor f => ( f t -> t ) -> Fix f -> t
cata phi (In ff) = phi (fmap (cata phi) ff)
para :: Functor f => ( f (Fix f, t) -> t) -> Fix f -> t
para psi (In ff) = psi (fmap (\x->(x,para psi x)) ff) 

para' psi = snd . cata (\x -> (In (fmap fst x), psi x))


data TreeF sub = 
      Leaf
    | Node sub Integer sub 

-- Exercise Define insertion for binary Search Tree,
-- first as a cata, then as a para.
-- You'll find the para version much easier than the cata version 



