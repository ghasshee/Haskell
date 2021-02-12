module Aop.Rec where 


import Aop.Int
import Aop.List





--{  Anamorphism   }--

ana                 :: (t->Bool) -> (t->(a,t)) -> t -> [a]  
ana p g t  | p t    = [] 
ana p g t           = a : (ana p g t')   where (a,t') = g t 

unfold              = ana 



-- e.g. 1
predicate_zip (as, bs)          = ( as == [] ) || ( bs == [] )
predesessor_zip (a:as, b:bs)    = ( (a,b), (as,bs) )

zip :: (Eq a, Eq b) => ([a],[b]) -> [(a,b)]
zip = ana p g 
    where 
        p = predicate_zip 
        g = predesessor_zip  

zip' :: (Eq a, Eq b) => [a] -> [b] -> [(a,b)]
zip' = curry ( ana p g )
    where 
        p = predicate_zip 
        g = predesessor_zip 

-- e.g. 2 
iterate :: (a->a) -> a -> [a]
iterate f = ana (const False) (prod id f) 

-- e.g. 3 
isZero 0    = True
isZero _    = False
pred'' n    = (n,pred n) 
hoge        = unfold isZero pred'' 10 





--{  Hylomorphism  }-- 
    
hylo h c p g    = (foldr h c) . (unfold p g) 


-- e.g. 
fac             = hylo (*) 1 isZero pred'' 




--{  Paramorphism  }--  

paran h c 0 = c 
paran h c n = h (pred n) (paran h c (pred n))


parar h c []        = c 
parar h c (a:as)    = h a as (parar h c as) 


-- e.g. 

fac' = paran (\n m -> (1+n)*m) 1 



-------------------------------------------
-- PARA ~~~ CATA 
--
-- paramorphism defined as catamoprhism
------------------------------------------


data Fix f = In (f (Fix f))

cata :: Functor f => ( f t -> t ) -> Fix f -> t
cata phi (In ff) = phi (fmap (cata phi) ff)
para :: Functor f => ( f (Fix f, t) -> t) -> Fix f -> t
para psi (In ff) = psi (fmap (prod id (para psi)) ff) 

para' psi = snd . cata (\x -> (In (fmap fst x), psi x))



-- Exercise : 
--      Define insertion for binary Search Tree,

type BinTree = Fix TreeF 

data TreeF sub = 
      Leaf
    | Node sub Integer sub 

-- first as a cata, then as a para.
-- You'll find the para version much easier than the cata version 
-- 
-- https://stackoverflow.com/questions/13317242/what-are-paramorphisms





prod :: (a->b) -> (a->c) -> a -> (b,c) 
prod f g a = (f a, g a) 


{--
         A 
       / | \ 
      f f,g g 
     /   |   \
    v    v    v
  B <-- BxC --> C
--} 
