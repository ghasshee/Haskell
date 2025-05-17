module Counter where

import Data.IORef 


data Counter = Counter { x :: IORef Int } 


makeCounter :: Int -> IO Counter 
makeCounter i = do 
    iref <- newIORef i
    return (Counter iref) 

incCounter :: Int -> Counter -> IO () 
incCounter i (Counter c) = do 
    modifyIORef c (+i) 

showCounter :: Counter -> IO ()
showCounter (Counter c) = do
    c' <- readIORef c 
    print c' 



-- test

a = makeCounter 1 

f x = do 
    a' <- a 
    incCounter 20 a' 
    showCounter a'
    return x 

  
-- >>> f 3 
-- >>> f 3 
-- This shows that IORef only work just in a single scope of IO. 
-- function f have no side effect. It's completely pure ! 

