module HexPrefix where 

import RLP
import Data.Word 
import Word4 

convert' = fromInteger . toInteger 
type Y = [Word4] 

hexprefix :: (Y,Int) -> B 
hexprefix (x,t) | even (length x)   = (16*f t) : loop x 
                | otherwise         = (16*(f t+1)+convert' (head x)):loop(tail x)   
                where 
                    f 0 = 0 
                    f _ = 2 
loop :: Y -> B 
loop [] = [] 
loop (x:y:xs) = convert' (16*x+y) : loop xs  




