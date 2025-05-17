{-# LANGUAGE CPP            #-} 
{-# LANGUAGE MagicHash      #-} 
{-# LANGUAGE UnboxedTuples  #-} 
module Word256 where 

import Prelude hiding (toInteger, Integral) 
import GHC.Prim
import GHC.Word 
import GHC.Types
import Data.Bits 
-- import GHC.Real 

import Data.Function (on)
import Foreign.C 
import Foreign.Ptr
import Foreign.Storable

import Basement.Compat.Base hiding (fromInteger) 
import Basement.Compat.Natural
import Basement.Compat.Primitive (bool#) 
import Basement.Numerical.Number
--import Basement.Numerical.Conversion 

data Word256 = Word256  {-# UNPACK #-} !Word64 
                        {-# UNPACK #-} !Word64 
                        {-# UNPACK #-} !Word64 
                        {-# UNPACK #-} !Word64 
                        deriving (Eq) 

instance Show Word256 where 
    show w = Prelude.show (toNatural w) 
        where   toNatural (Word256 w1 w2 w3 w4) = (i w1)*(2^192) + (i w2)*(2^128) + (i w3)*(2^64) + i w4 
                i = toInteger 

instance Enum Word256 where 
    toEnum i = Word256 0 0 0 $ fromInteger (toInteger i)
    fromEnum (Word256 _ _ _ w) = fromInteger (toInteger w) 
    pred (Word256 w3 w2 w1 w0) 
        | w0 == minBound    =   if w1 == minBound 
                                    then if w2 == minBound 
                                            then Word256 (pred w3) maxBound maxBound maxBound 
                                            else Word256 w3 (pred w2) maxBound maxBound 
                                    else Word256 w3 w2 (pred w1) maxBound     
        | otherwise         =   Word256 w3 w2 w1 (pred w0) 
    succ (Word256 w3 w2 w1 w0) 
        | w0 == maxBound    =   if w1 == maxBound 
                                    then if w2 == maxBound 
                                            then Word256 (succ w3) 0 0 0 
                                            else Word256 w3 (succ w2) 0 0 
                                    else Word256 w3 w2(succ w1) 0
        | otherwise         =   Word256 w3 w2 w1 (succ w0)

instance Bounded Word256 where 
    maxBound = Word256 maxBound maxBound maxBound maxBound 
    minBound = Word256 minBound minBound minBound minBound 

instance Ord Word256 where 
    compare (Word256 a3 a2 a1 a0) (Word256 b3 b2 b1 b0) = compareEq a3 b3 $ compareEq a2 b2 $ compareEq a1 b1 $ compare a0 b0 
        where   compareEq a b next = case compare a b of 
                    EQ -> next 
                    r  -> r 
    (<)     (Word256 a3 a2 a1 a0) (Word256 b3 b2 b1 b0) = compareLT a3 b3 $ compareLT a2 b2 $ compareLT a1 b1 $ (<)     a0 b0 
        where   compareLT a b next = case compare a b of 
                    EQ -> next 
                    r  -> r == LT 

instance Storable Word256 where 
    sizeOf _    = 32 
    alignment _ = 32 
    peek p      = Word256   <$> peek (castPtr p ) 
                            <*> peek (castPtr p `plusPtr` 8)
                            <*> peek (castPtr p `plusPtr` 16)
                            <*> peek (castPtr p `plusPtr` 24)
    poke p (Word256 a3 a2 a1 a0) = do 
        poke (castPtr p             ) a3
        poke (castPtr p `plusPtr` 8 ) a2
        poke (castPtr p `plusPtr` 16) a1
        poke (castPtr p `plusPtr` 24) a0

instance Integral Word256 where
    fromInteger = literal 



