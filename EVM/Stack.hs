module Stack where

import Control.Monad.State
import Syntax 

{--
data Stack a = Stack [a] 

push :: Stack a -> a -> Stack a 
push (Stack l) x = Stack (x:l) 

empty :: Stack a 
empty = Stack []

pop :: Stack a -> (a, Stack a) 
pop (Stack [])      = error "StackUnderFlow"
pop (Stack (x:xs))  = (x, Stack xs)   
--} 



type Stack a = [a] 





push :: a -> State (Stack a) () 
push x = state $ \xs -> ((), x:xs) 

pop :: State (Stack a) a 
pop = state $ (\(x:xs) -> (x,xs)) 

popTwice :: State (Stack a) [a]
popTwice = do 
    a <- pop 
    b <- pop 
    return [a,b] 

add :: Num a => State (Stack a) () 
add = do 
    a <- pop 
    b <- pop 
    push (a+b) 
    return ()




