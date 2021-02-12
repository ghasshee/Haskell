{-# LANGUAGE DeriveAnyClass #-} 


module DataTypes where 

import Prelude hiding ((+),(*),(-),(/)) 
import Basement.Types.Word256 


{-- 
instance Show Word256 where 
    show w = Prelude.show (toNatural w) 
        where   toNatural (Word256 w1 w2 w3 w4) = (i w1)*(2^192) + (i w2)*(2^128) + (i w3)*(2^64) + i w4 
                i = toInteger 

data U256 = U256  {-# UNPACK #-} !Word256 
                        deriving (Show, Eq) 

--} 


type U256 = Word256 

