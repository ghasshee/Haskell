module Trie where 


import RLP 
import HexPrefix 
import Crypto 


napp 0 f x = x 
napp n f x = f (napp (n-1) f x)


type Trie   = [(B,B)] 
type Trie'  = [(Y,B)] 

-- plp :: T -> B 
-- hexprefix :: (Y,Int) -> B 

n :: (Trie',Int)  -> B  
n ([],i)                            = []
n (tr,i)    | (length.c)(tr,i)<32   = c(tr,i)  
            | otherwise             = keccak (c(tr,i)) 


c :: (Trie',Int)  -> B 
c ([(i0,i1)] , i) = rlp $ Leaf $ hexprefix (napp i tail i0, 1) ++ i1 

