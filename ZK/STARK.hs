module STARK where 
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

import Crypto.Hash 
import Data.ByteString  (ByteString) 
import Data.ByteArray   (convert) 

import Control.Exception (assert)


sha256          ::  ByteString -> ByteString 
sha256 bs       =   convert (hash bs :: Digest SHA256) 

hash256         ::  ByteString -> ByteString 
hash256         =   sha256 . sha256 

ripemd160       ::  ByteString -> ByteString 
ripemd160 bs    =   convert (hash bs :: Digest RIPEMD160) 

hash160         ::  ByteString -> ByteString 
hash160         =   ripemd160 . sha256

blake           ::  ByteString -> ByteString 
blake   bs      =   convert (hash bs :: Digest Blake2s_256)


data PrimeField = F Integer  deriving (Show, Ord, Eq )

len = length 
rev = reverse 

p = 337
--p = 2^256 - 351 * 2^32 + 1

instance Num PrimeField where 
    F n + F m       = F $ (n + m) `mod` p 
    F n * F m       = F $ (n * m) `mod` p
    F n - F m       = F $ (n - m) `mod` p 
    fromInteger n   = F n 
    abs x           = x
    signum x        = 1

instance Fractional PrimeField where 
    fromRational    = undefined
    recip (F 0)     = F 0 
    recip (F n)     = F $ (loop (n `mod` p) p 1 0) `mod` p  
                        where   loop l h lm hm | l<=1   = lm
                                loop l h lm hm          = loop n l nm lm 
                                    where   r   = h `div` l 
                                            n   = (h-l*r)
                                            nm  = (hm-lm*r) 
                                                
instance Real PrimeField where 
    toRational  = undefined 

instance Enum PrimeField where 
    toEnum      = undefined 
    fromEnum    = undefined 

instance Integral PrimeField where 
    F n `div` F m       = F n * (recip (F m)) 
    toInteger (F n)     = n 
    quotRem (F n)(F m)  = (F n',F m') where (n',m') = quotRem n m 

class Integral f => Polynomial f where 
    multi_inv   :: [f] -> [f]
    eval        :: [f] ->  f  ->  f  
    inv         ::  f  ->  f
    (+$)        :: [f] -> [f] -> [f] 
    (-$)        :: [f] -> [f] -> [f]
    (*$)        :: [f] -> [f] -> [f]
    (/$)        :: [f] -> [f] -> ([f],[f])
    zpoly       :: [f] -> [f] 
    lagrange    :: [f] -> [f] -> [f] 


-- multiple inversion algorithms : m1 m2 m3 
m1 []                       = [] 
m1 (x:xs)                   = x : map ((*)x) (m1 xs) 

m2 [a_z] [z]                = [recip a_z]
m2 (ab:abcs) (b:c:ds)       = iabc*c : iabc : invs
                                where  iabc : invs = m2 abcs (c:ds) 
m3 [iy,ia_z] [a_y,a_z]      = [iy, a_y*ia_z]    
m3 (ia:iab:iabcs)(a:abs)    = ia : m3 (a*iab:iabcs) abs  

-- polynomial devision algorithm
pdiv (x:xs) (y:ys) quot     = if len xs < len ys 
                                then (quot, rev (x:xs)) 
                                else pdiv xs' (y:ys) (q:quot) 
                                    where   q   = x `div` y 
                                            ys' = map ((*)q) ys 
                                            xs' = xs -$ ys'


instance Polynomial PrimeField where 
    multi_inv values =  m3 (m2 (m1 values) values) (m1 values)
    eval []     v    =  F 0 
    eval (x:xs) v    =  x + eval (map ((*)v) xs) v
    inv              =  recip 
    xs     +$ []     =  xs 
    []     +$ ys     =  ys 
    (x:xs) +$ (y:ys) =  x+y : xs +$ ys 
    xs     -$ []     =  xs 
    []     -$ ys     =  ys 
    (x:xs) -$ (y:ys) =  x-y : xs -$ ys 
    xs     *$ []     =  []
    xs     *$ (y:ys) =  (map ((*)y) xs) +$ ((F 0:xs) *$ ys )
    xs     /$ ys     =  pdiv (rev xs) (rev ys) []

    zpoly []         = [1]
    zpoly (v:vs)     = [- v,1] *$ zpoly vs   

    lagrange xs ys   = sumup ys invdenoms nums  
        where   root    = zpoly xs
                nums    = loop xs root 
                    where   loop [] root        = []
                            loop (v:vs) root    = q : loop vs root 
                                where (q,_) = root /$ [- v,1] 
                denoms  = loop xs nums 
                    where   loop [] nums        = []
                            loop (v:vs)(n:ns)   = eval n v : loop vs ns 
                invdenoms = multi_inv denoms 
                sumup [] invs nums          = [F 0] 
                sumup (y:ys)(i:invs)(n:ns)  = (map (\v-> y*i*v) n) +$ (sumup ys invs ns)      




fft [v]  zeta     = [v] 
fft vals zeta     = o1 ++ o2 
    where   
            (o1,o2)             = unzip $ loop l r 0 
            loop [] _ _         = [] 
            loop (x:l)(y:r)i    = (x+yi,x-yi) : loop l r (i+1)
                    where yi        = y * zeta ^ F i 
            l                   = fft ls (zeta^F 2) 
            r                   = fft rs (zeta^F 2)
            (ls,rs)             = unzip $ split vals   
            split []            = [] 
            split (l:r:s)       = (l,r) : split s 

-- inv_fft vals zeta = foldr ((.)(.)(.)(:)(*)(inv . F . toInteger . len $ vals)) [] (fft vals (inv zeta))
inv_fft vals zeta = loop (fft vals (inv zeta))
    where   
            loop []             = [] 
            loop (x:xs)         = x * invlen : loop xs 
            invlen              = (inv . F . toInteger . len) vals



get_power_cycle zeta    = loop zeta [F 1]
    where 
            loop (F 1)  cycle   =   rev cycle
            loop zeta_k cycle   =   loop (zeta_k*zeta) (zeta_k:cycle) 


prove_low_degree values zeta maxdeg_plus_1 = do 
    print ""

    if maxdeg_plus_1 <= 16 
        then return values else do 

    if len values == len xs 
        then error ""  else do 



    return values 
    


zeta = undefined 
xs   = get_power_cycle zeta 





--import GHC.TypeLits
--import Data.Modular
--
--import qualified Data.Vector.Fixed as Fix
--import qualified Data.Vector.Fixed.Boxed as Fix
--
--data GF χ -- characteristic
--        n -- power
--   = GF { irreducible :: [ℤ/χ]
--        , poly :: [ℤ/χ]
--        }
--
--
--
--data GF χ n
--   = GF { irreducible :: Fix.Vec (n+1) (ℤ/χ)
--        , poly :: Fix.Vec (n+1) (ℤ/χ)
--        }
--deriving instance (KnownNat χ, Fix.Arity (n+1)) => Show (GF χ n)
--
--addGF :: (KnownNat χ, Fix.Arity (n+1))
--           => GF χ n -> GF χ n -> GF χ n
--addGF (GF irr xp) (GF irr' yp)
-- | irr==irr'  = GF irr $ Fix.zipWith (+) xp yp
-- | otherwise  = error "Cannot add elements of finite fields with different irreducible polynomials!"
--
--main = print (GF irr (Fix.fromList [0,0,1]) `addGF` GF irr (Fix.fromList [0,1,1])
--               :: GF 2 2)
