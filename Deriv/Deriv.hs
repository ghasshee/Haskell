{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{--

-- D is Double Number 

data D a = D a a 
        deriving Show 

real, grad      :: D a -> a 
real (D x _ )   = x
grad (D _ x')   = x' 


instance Num a => Num (D a) where 
    (D x x') + (D y y')     = D (x+y)(x'+y')
    (D x x') * (D y y')     = D (x*y)(x*y'+y*x')
    negate  (D x x')        = D (negate x)(negate x')
    abs     (D x x')        = D (abs x)(x'*(signum x))
    signum  (D x x')        = D (signum x) 0
    fromInteger n           = D (fromInteger n) 0

instance Fractional a => Fractional (D a) where
    recip (D x x')          = D (recip x)(-1 * x' * (recip (x*x)))
    fromRational x          = D (fromRational x) 0

instance Floating a => Floating (D a) where 
    pi                      = D pi 0
    exp     (D x x')        = D (exp   x)( x' * exp x       )
    log     (D x x')        = D (log   x)( x' / x           )
    sin     (D x x')        = D (sin   x)( x' * cos x       )
    cos     (D x x')        = D (cos   x)(-x' * sin x       )
    asin    (D x x')        = D (asin  x)( x' / sqrt(1-x**2))
    acos    (D x x')        = D (acos  x)(-x' / sqrt(1-x**2))
    atan    (D x x')        = D (atan  x)( x' / sqrt(1+x**2))
    sinh    (D x x')        = D (sinh  x)( x' * cosh x      )
    cosh    (D x x')        = D (cosh  x)( x' * sinh x      )
    asinh   (D x x')        = D (asinh x)( x' / sqrt(1+x**2))
    acosh   (D x x')        = D (acosh x)( x' / sqrt(x**2-1))
    atanh   (D x x')        = D (atanh x)( x' / (1-x**2)    )
--}



data Dif a = D { dVal :: a, deriv :: Dif a } 
        deriving Show 


dConst          :: Num a => a -> Dif a 
dConst x0       = D x0 dZero 

dZero           :: Num a => Dif a 
dZero           = D 0 dZero 


instance Num a => Num (Dif a) where 
    D x x' + D y y'         = D (x+y)(x'+y')
    D x x'  * D y y'        = D (x*y)(x' * D y y' + y' * D x x') 
    negate (D x x')         = D (negate x) (negate x') 
    signum (D x x')         = D (signum x) dZero
    fromInteger n           = D (fromInteger n) dZero
    abs    (D x x')         = D (abs x) (signum(dConst x) * x') 

dlift :: Num a => (a->a) -> (Dif a->Dif a) -> Dif a -> Dif a
dlift f f'  (D x x') = D (f x) (f' (D x x') * x') 
    
inftx 0 >-< 
(>-<) = dlift

