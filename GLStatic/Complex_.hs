{-# LANGUAGE TypeSynonymInstances  #-} 
{-# LANGUAGE FlexibleInstances #-} 

module Complex_ where

type Complex                = (Double,Double) 

one                         = (1,0) 
two                         = (2,0)
i                           = (0,1)
re          (x,y)           = x
im          (x,y)           = y
conj        (x,y)           = (x,-y) 
theta       (x,y)           = atan (y/x) 
reciprocal  (x,y)           = (1,0)/(x,y) :: Complex

toPole    z                 = log z
fromPole  (r,θ)             = exp (r,θ) :: Complex 
pow w                       = exp . (*)w . log 

instance {-# OVERLAPPING #-} Show Complex where 
    show (x,0)              = show x
    show (0,y)              = show y ++ "i" 
    show (x,y) | y<0        = show x ++ show y ++ "i" 
    show (x,y)              = show x ++ "+" ++ show y ++ "i"  

instance Num Complex where
    (x,y) * (a,b)           = (x*a-y*b,x*b+y*a)
    (x,y) + (a,b)           = (x+a,   y+b)
    (x,y) - (a,b)           = (x-a,   y-b) 
    abs (x,y)               = (sqrt(x^2 +y^2),0)
    signum (x,y)            = (signum x,0)
    fromInteger x           = (fromInteger x,0)

infinity = 10000000000000000000000000000000000000000000 :: Double 

instance Fractional Complex where 
    (x,y)/(0,0)             = (infinity,infinity)
    (x,y)/(a,b)             = ((x*a+y*b)/(a^2+b^2), (-x*b+y*a)/(a^2+b^2))
    fromRational x          = (fromRational x, 0)

instance Floating Complex where 
    exp (x,y)               = (exp x, 0) * (cos y,sin y)
    sin z                   = ((exp(i*z)) - exp(-i*z)) / 2*i
    cos z                   = ((exp(i*z)) + exp(-i*z)) / two
    sinh z                  = ((exp(z)) - exp(-z)) / two
    cosh z                  = ((exp(z)) + exp(-z)) / two
    pi                      = (pi,0)
    log (x,y) | x>=0        = (log r,if y>=0 then theta else 2*pi+theta) 
        where   r               = re $ abs(x,y) 
                theta           = atan (y/x)
    log (x,y)               = (log r,pi+theta) 
        where   r               = re $ abs(x,y) 
                theta           = atan (y/x)
    atan z                  = (0,(1/2)) * log ((one-i*z)/(one+i*z))
    asin (x,0)              = (asin x, 0)    
    asin z                  = -i * log (i*z+sqrt(one-z^2))
    acos (x,0)              = (acos x, 0)
    acos z                  = -i * log (z+i*sqrt(one-z^2))
    atanh z                 = (1/2,0) * log((z+one)/(z-one))
    asinh z                 = log(z + sqrt(z^2+one))
    acosh z                 = log(z + sqrt(z^2-one))


