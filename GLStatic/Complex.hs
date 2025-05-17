module Complex where

data Complex                =   C Double Double 
                            |   Infinity deriving (Eq,Ord)

re          (C x y)         = x
im          (C x y)         = y
conj        (C x y)         = C x(-y) 
theta       (C x y)         = atan (y/x) 
e                           = exp
π                           = pi
θ                           = theta 
zero                        = C 0 0 
one                         = C 1 0 
two                         = C 2 0
i                           = C 0 1
half                        = C(1/2)0
reciprocal Infinity         = zero
reciprocal  z               = one/z
toPoler                     = log
fromPoler                   = exp 
pow w                       = fromPoler . (*) w . toPoler  

instance Show Complex where 
    show (C x 0)            = show x
    show (C 0 y)            = show y ++ "i" 
    show (C x y) | y<0      = show x ++ show (C 0 y) 
    show (C x y)            = show x ++ "+" ++ show(C 0 y) 

instance Num Complex where
    (C x y) * (C a b)       = C (x*a-y*b) (x*b+y*a)
    (C 0 0) * Infinity      = C 0 0  
    Infinity * (C 0 0)      = C 0 0 
    _ * _                   = Infinity 
    (C x y) + (C a b)       = C (x+a)     (y+b)
    (C x y) - (C a b)       = C (x-a)     (y-b) 
    abs (C x y)             = C (sqrt(x^2 +y^2)) 0
    signum (C x y)          = C (signum x) 0
    fromInteger x           = C (fromInteger x) 0

instance Fractional Complex where 
    (C x y)/(C 0 0)         = Infinity 
    (C x y)/(C a b)         = C((x*a+y*b)/(a^2+b^2))((-x*b+y*a)/(a^2+b^2))
    fromRational x          = C(fromRational x) 0

instance Floating Complex where 
    exp (C x y)             = C(exp x)0 * C(cos y)(sin y)
    sin z                   = (exp(i*z) - exp(-i*z)) / two*i
    cos z                   = (exp(i*z) + exp(-i*z)) / two
    sinh z                  = (exp(z)   - exp(-z))   / two
    cosh z                  = (exp(z)   + exp(-z))   / two
    pi                      = C π 0
    log z  | re z >=0       = C(log r)(if im z>=0 then θ else θ+2*π)
        where   r               = re $ abs z  
                θ               = theta z 
    log z                   = C(log r)(π+θ)
        where   r               = re $ abs z
                θ               = theta z  
    atan z                  = C 0(1/2) * log ((one-i*z)/(one+i*z))
    asin (C x 0)            = C (asin x) 0    
    asin z                  = -i*log (i*z+sqrt(one-z^2))
    acos (C x 0)            = C (acos x) 0
    acos z                  = -i*log (z+i*sqrt(one-z^2))
    atanh z                 = C(1/2)0*log((z+one)/(z-one))
    asinh z                 = log(z+sqrt(z^2+one))
    acosh z                 = log(z+sqrt(z^2-one))
