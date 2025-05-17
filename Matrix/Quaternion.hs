module Quatarnion where

data Q a = Q a a a a

show' x = if (abs x) == x then "+" ++ show x else show x 
instance (Show a, Num a, Eq a) => Show (Q a) where
    show (Q x y z w) = show x ++ show' y ++ "i" ++ show' z ++ "j" ++ show' w ++ "k"

instance (Num a) => Num (Q a) where
    (Q x y z w)+(Q a b c d) = Q(x+a)(y+b)(z+c)(w+d)
    (Q x y z w)-(Q a b c d) = Q(x-a)(y-b)(z-c)(w-d)
    (Q x y z w)*(Q a b c d) = 
        Q(x*a-y*b-z*c-w*d)(x*b+y*a+z*d-w*c)(x*c+z*a+w*b-y*d)(x*d+w*a+y*c-z*b)
    abs     (Q x y z w)     = Q (abs x)(abs y)(abs z)(abs w)
    signum  (Q x y z w)     = Q(signum x)(signum y)(signum z)(signum w)
    negate  (Q x y z w)     = Q(n x)(n y)(n z)(n w) where n = negate
    fromInteger n = let zero = fromInteger 0 in Q (fromInteger n)zero zero zero


e = Q 1 0 0 0  
i = Q 0 1 0 0
j = Q 0 0 1 0
k = Q 0 0 0 1

h = i + j + k

