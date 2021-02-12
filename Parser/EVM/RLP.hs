module RLP where 


import Data.Word


type O = Word8 
type B = [O] 


data T = Node [T] | Leaf B 

convert = fromInteger . toInteger 

rlp :: T -> B
rlp (Leaf x)    = r_b x 
rlp (Node x)    = r_l x 

r_b :: B -> B 
r_b x       | length x == 1 && head x < 128 = x
            | length x < 56                 = ((128 :: O) + (convert . length) x) : x 
            | length x < 2^63 {-- 2^64 --}  = ((183 :: O) + convert (length (big_endian (length x)))) : big_endian (length x) ++ x  
            | otherwise                     = []

big_endian :: Int -> B  
big_endian 0 = []
big_endian x = (convert (mod x 256)) : (big_endian (div x 256))  


r_l :: [T] -> B 
r_l x       | s x /= [] && length(s x)< 56  = ((192 :: O) + (convert . length)(s x)) : s x 
            | s x /= [] && length(s x)<2^63 = ((247 :: O) + convert (length (big_endian (length (s x))))) : big_endian (length(s x)) ++ s x


s x = case s' x of 
    Just x      -> concat x 
    Nothing     -> [] 
    where 
        s' []                       = Just []
        s' (x:xs) | rlp x /= []     = return . (:) (rlp x) =<< s' xs 
        s' (x:xs) | rlp x == []     = Nothing 
 

rlp_n :: Int -> B  
rlp_n = r_b . big_endian 

