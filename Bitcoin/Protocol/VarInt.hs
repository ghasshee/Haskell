{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bitcoin.Protocol.VarInt where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Binary (Binary(..))
import Data.Binary.Put(putWord8,putWord16le,putWord32le,putWord64le)
import Data.Binary.Get(getWord8,getWord16le,getWord32le,getWord64le)

newtype VarInt = VarInt Word64
    deriving (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral)

max16 :: Word16
max16 = maxBound
max32 :: Word32
max32 = maxBound


instance Binary VarInt where
    put x 
        | x < 0xFD                  = (putWord8 . fromIntegral) x
        | (fromIntegral x)<=max16   = putWord8 0xFD >> putWord16le(fromIntegral x)
        | (fromIntegral x)<=max32   = putWord8 0xFD >> putWord32le(fromIntegral x)
        | otherwise                 = putWord8 0xFD >> putWord64le(fromIntegral x)
    get = VarInt <$> (getWord8 >>= getValue)
        where
            getValue size 
                | size <    0xFD    = pure (fromIntegral size)
                | size ==   0xFD    = fromIntegral <$> getWord16le
                | size ==   0xFE    = fromIntegral <$> getWord32le
                | otherwise         = fromIntegral <$> getWord64le
