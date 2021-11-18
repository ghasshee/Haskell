module Cont.Cont where 


-- for test 
import Data.Char (chr , ord) 


newtype Cont k a = Cont {runCont :: ((a->k)->k) } 
-- runCont  :: Cont k a -> (a->k) -> k 
-- Cont     :: ((a->k)->k) -> Cont k a 

instance Functor (Cont r) where 
    fmap f          = undefined

instance Applicative (Cont r) where
    (<*>)           = undefined  
    pure            = return 

instance Monad (Cont r) where 
    return x        = Cont $ \k -> k x 
    (Cont n) >>= m  = Cont $ \k -> n (\n' -> runCont (m n') k) 
    -- (x)*    k    = k x 
    -- (m* n)*  k   = m* n k 
    --              = n* (\n' -> (m* n') k) 



class (Monad m) => MonadCont m where 
    callCC :: ((a -> m b) -> m a) -> m a 

instance MonadCont (Cont r) where 
    callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k 
    

-- e.g. 
--
calculateLength :: [a] -> Cont r Int 
calculateLength l = return (length l) 
-- runCont (calculateLength "123" ) print
hoge = do 
    runCont (calculateLength "123364871932671829317238091" >>= pure . (*2) >>= pure . chr ) print

double n = return (n*2) 
-- runCont (calculateLength "123" >>= double) print



{- using Continuation, we break out of a block of codes 
 -
 - Input(n)         Output                          List Shown
 - ===============  =============================   ====================
 - 0-9              n                               none
 - 10-199           number of digits in (n/2)       digits of (n/2) 
 -
 -
 -}

fun :: Int -> String 
fun = undefined 
