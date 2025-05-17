module Ref where 



incr = \f -> (\x -> x + 1) . f 

decr = \f -> (\x -> x - 1) . f 
