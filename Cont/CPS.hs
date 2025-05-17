{-# LANGUAGE UnicodeSyntax #-} 
{-# LANGUAGE BangPatterns  #-} 

module Cont.CPS where


type CPS a' =   ∀ k . (a' -> k) -> k 

toCPS       ::  a -> CPS a   
toCPS       =   flip ($) 

fromCPS     ::  CPS a -> a 
fromCPS     =   ($ id)            -- fromCPS k = k id 

callCC :: ((a->(b->k)->k)->(a->k)->k) -> (a->k) ->         k 
callCC              f                      h    =   f (\a _->h a) h




{- #################################
 - ##   Associativity of Monoid   ##
 - ################################# -} 

chainCPS :: ((a->k)->k) -> (a->((b->k)->k)) -> (b->k) -> k
chainCPS s f  = s . flip f  
(<->) = chainCPS 

monoidToCPS :: Monoid a => a -> (()->a) -> a 
monoidToCPS a = (a <>) . ($ ())

monoidFromCPS :: Monoid a => ((() -> a) -> a) -> a 
monoidFromCPS cps = cps (const mempty) 

-- e.g. 
sumL    = ([1,2,3] <> [4,5,6]) <> [7,8,9] 
sumR    = monoidFromCPS $ monoidToCPS [1,2,3] <-> (\_ -> monoidToCPS [4,5,6] <-> (\_ -> monoidToCPS [7,8,9]))
sumR'   = [1,2,3] <> ([4,5,6] <> [7,8,9])



{- ##################################
 - ##        Monad / CPS           ##
 - ################################## -} 

monadToCPS :: Monad m => m a -> (forall k. (a -> m k) -> m k) 
monadToCPS ma = (ma >>=) 

monadFromCPS :: Monad m => (forall k. (a->m k)->m k) -> m a 
monadFromCPS cps = cps pure 

-- e.g. 
resL    = [1,2,3] >>= (\x -> [x+1]) >>= (\y -> [y-2]) 
resL'   = [1,2,3] >>= (\x -> [x+1]) >>= (\y -> [y-2]) 
resR    = [1,2,3] >>= (\x -> [x+1] >>= (\y -> [y-2]))
resR'   = monadFromCPS $ monadToCPS [1,2,3] <-> (\x -> monadToCPS [x+1]) <-> (\y -> monadToCPS [y-2])  






fac :: Integer -> Integer 
fac 0 = 1 
fac n = n * fac (n-1) 
-- >>> fac n 
-- |->   n * (n-1) * ... * 2 * 1 * 1



facCPS :: Integer -> (Integer -> k) -> k 
facCPS 0 k = k 1 
facCPS n k = facCPS (n-1) $ k . (*n) 
-- >>> facCPS 10 id 
-- |->  ((...(( id . (*n) ) . (*(n-1) )) . ... ) . (*2) ) 1 
-- |->  (*n) . ... . (*2) . (*1) $ 1 



facIterative :: Integer -> Integer 
facIterative = go 1 
    where 
        go !acc 0 = acc
        go !acc n = go (acc*n) (n-1) 
-- Much Faster 





data BTree = Br BTree BTree | Lf Int


lfSum (Lf x)        = x 
lfSum (Br l r)      = lfSum l + lfSum r 

lfSumCPS (Lf x) k   = k x 
lfSumCPS (Br l r) k = lfSumCPS l ( \l' -> lfSumCPS r (\r' -> k (l'+r')))


-- if  there is a lf which has value 6, return 1000
-- else return sum

lfSumCPS' (Lf 6) k  = 1000
lfSumCPS' (Lf x) k  = k x
lfSumCPS' (Br l r)k = lfSumCPS' l ( \l' -> lfSumCPS' r (\r' -> k (l'+r')))

-- ## CONCLUSION ## 
-- With CPS, 
-- we can quit the tree search at any points which we want . 







