module C where

data Complex a = C a a | Infinity deriving (Eq,Ord)


instance Show a => Show (Complex a) where 
    show (C x y)        = if y>0 && x/=0 
                            then show x ++ "+" ++ show y ++ "i"
                            else if y==0 
                                then show x
                                else if x==0 
                                    then show y ++ "i" 
                                    else show x ++ show y ++ "i"

instance Functor Complex where 
    fmap f (C a b)      = C (f a) (f b)


instance Num a => Num (Complex a) where
    (C x y) * (C a b)   = C (x*a-y*b) (x*b+y*a)
    (C x y) + (C a b)   = C (x+a)     (y+b)
    (C x y) - (C a b)   = C (x-a)     (y-b) 
    abs (C x y)         = C (sqrt $ x*x + y*y) 0
    signum (C x y)      = C (signum x) 0
    fromInteger x       = C (fromInteger x) 0

instance Fractional a => Fractional (Complex a) where 
    (C x y) / (C r s)   = if r==zero && s==zero then C.Infinity
                            else C ((x*r+y*s)/(r^2+s^2)) ((-x*s+y*r)/(r^2+s^2))
    fromRational x      = C (fromRational x) 0

zero :: (Num a) => a
zero = fromInteger 0

instance Floating a => Floating (Complex a) where
    exp (C x y)         = C (exp x) 0  * C(cos y)(sin y)
    sin z               = ((exp (C 0 1*z)) - exp (-C 0 1*z)) / (C 0 2)
    cos z               = ((exp (i*z)) + exp (-i*z)) / 2
    sinh z              = ((exp (z)) - exp (-z)) / 2
    cosh z              = ((exp (z)) + exp (-z)) / 2
    pi                  = pi
    log (C x y)         = C (log r)  theta where 
        r                   = re $ abs (C x y) 
        theta               = atan ( y/x )
    atan z              = C 0(1/2) * log ((1+i*z)/(1-i*z))
    asin z              = if y==0   then C (asin x) 0
                                    else -i * log (i*z+sqrt(1-z^2))
                                        where C x y = z
    acos z              = if y==0   then C (acos x) 0
                                    else -i * log (z+i*sqrt(1-z^2))
                                        where C x y = z
    atanh z             = C(1/2)0 * log((z+1)/(z-1))
    asinh z             = log(z + sqrt(z^2+1))
    acosh z             = log(z + sqrt(z^2-1))

i = C 0 (fromInteger 1)
one             = C 1 0 
two             = C 2 0
re (C x y)      = x
im (C x y)      = y
conj    (C x y) = C x (-y) 
reciprocal z    = (C 1 0) / z

