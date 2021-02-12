{-# LANGUAGE DeriveDataTypeable     #-} 
{-# LANGUAGE FlexibleInstances      #-} 
{-# LANGUAGE MultiParamTypeClasses  #-} 
module Word4 where 

import Prelude  

import GHC.Enum (succError, predError) 
import Data.Generics (Data,Typeable) 
import GHC.Word
import Data.Bits 


newtype Word4 = Word4 Word8 
    deriving (Eq, Ord, Data, Typeable) 


word4 :: Word8 -> Word4 
word4 x = Word4 (x .&. 0x0f) 

instance Show Word4 where 
    show (Word4 x)      = show x

instance Bounded Word4 where 
    minBound = Word4 0x00
    maxBound = Word4 0x0f 

instance Enum Word4 where 
    succ (Word4 x) = if x < 0x0f then Word4 (succ x) else succError "Word4"
    pred (Word4 x) = if x > 0x00 then Word4 (pred x) else predError "Word4" 
    toEnum i    | 0x00 <= i && i <= 0x0f    = Word4 (toEnum i)
                | otherwise                 = error "toEnum Error: out of bounds"
    fromEnum (Word4 x) = fromEnum x 

instance Num Word4 where 
    Word4 x + Word4 y = word4 (x+y) 
    Word4 x - Word4 y = word4 (x-y) 
    Word4 x * Word4 y = word4 (x*y) 
    negate (Word4 x)  = word4 (negate x) 
    abs (Word4 x)     = Word4 x
    signum (Word4 x)  = Word4 (if x==0 then 0 else 1) 
    fromInteger n     = word4 (fromInteger n) 

instance Real Word4 where 
    toRational (Word4 x) = toRational x

instance Integral Word4 where 
    quotRem (Word4 x)(Word4 y) = (Word4 q, Word4 r) 
        where (q,r) = quotRem x y 
    toInteger (Word4 x) = toInteger x

instance Bits Word4 where 
    Word4 x .&. Word4 y = Word4 (x .&. y) 
    Word4 x .|. Word4 y = Word4 (x .|. y) 
    Word4 x`xor`Word4 y = Word4 (x`xor`y) 
    complement(Word4 x) = Word4 (x `xor` 0x0f) 
    Word4 x `shift`  i  = word4 (shift  x i) 
    Word4 x `shiftL` i  = word4 (shiftL x i) 
    Word4 x `shiftR` i  = word4 (shiftR x i) 
    Word4 x `rotate` i  = word4 (x `shiftL` k .|. x `shiftR` (4-k))
        where k = i .&. 3
    bitSize _           = 4 
    bitSizeMaybe _      = Just 4 
    isSigned _          = False 
    testBit (Word4 x)   = testBit x
    bit i               = word4 (bit i)
    popCount (Word4 x)  = popCount x



