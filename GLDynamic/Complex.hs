module Complex where

data Complex = C Double Double | Infinity deriving (Eq,Ord)
instance Show Complex where 
    show (C x y)        = if y>0 && x/=0 
                            then show x ++ "+" ++ show y ++ "i"
                            else if y==0 
                                then show x
                                else if x==0 
                                    then show y ++ "i" 
                                    else show x ++ show y ++ "i"
            
instance Num Complex where
    (C x y) * (C a b)   = C (x*a-y*b) (x*b+y*a)
    (C x y) + (C a b)   = C (x+a)     (y+b)
    (C x y) - (C a b)   = C (x-a)     (y-b) 
    abs (C x y)         = C (sqrt $ x*x + y*y) 0
    signum (C x y)      = C (signum x) 0
    fromInteger x       = C (fromInteger x) 0

instance Fractional Complex where 
    (C x y) / (C a b)   = if a==0 && b==0 then Infinity
                            else C ((x*a+y*b)/(a^2+b^2)) ((-x*b+y*a)/(a^2+b^2))
    fromRational x      = C (fromRational x) 0

instance Floating Complex where 
    exp (C x y)         = C (exp x) 0  * C(cos y)(sin y)
    sin z               = ((exp (i*z)) - exp (-i*z)) / 2*i
    cos z               = ((exp (i*z)) + exp (-i*z)) / two
    sinh z              = ((exp (z)) - exp (-z)) / two
    cosh z              = ((exp (z)) + exp (-z)) / two
    pi                  = pi
    log (C x y)         = C (log r)  theta where 
        r                   = re $ abs (C x y) 
        theta               = atan ( y/x )
    atan z              = C 0(1/2) * log ((one-i*z)/(one+i*z))
    asin z              = if y==0   then C (asin x) 0
                                    else -i * log (i*z+sqrt(one-z^2))
                                        where C x y = z
    acos z              = if y==0   then C (acos x) 0
                                    else -i * log (z+i*sqrt(one-z^2))
                                        where C x y = z
    atanh z             = C(1/2)0 * log((z+one)/(z-one))
    asinh z             = log(z + sqrt(z^2+one))
    acosh z             = log(z + sqrt(z^2-one))
one             = C 1 0 
two             = C 2 0
i               = C 0 1
re (C x y)      = x
im (C x y)      = y
conj    (C x y) = C x (-y) 
reciprocal z    = (C 1 0) / z

cart2pole (C x y)   = (sqrt(x^2+y^2),atan(y/x))
pole2cart (r,θ)     = (C r 0) * (exp (C 0 θ))
power_i             = pole2cart . (\(x,y)-> (-y,x)) . cart2pole
