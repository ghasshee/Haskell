{-# LANGUAGE GeneralizedNewtypeDeriving #-} 

module Bitcoin.VarInt where 

import Bitcoin.Types
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get


newtype VarInt = VarInt Word64
    deriving (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral)

instance Binary VarInt where 
    put x   | x < 0xFD                      = putWord8 (fromIntegral x)
            | fromIntegral x <= (maxBound::Word16) = putWord8 0xFD >> putWord16le (fromIntegral x)
            | fromIntegral x <= (maxBound::Word32) = putWord8 0xFe >> putWord32le (fromIntegral x) 
            | fromIntegral x <= (maxBound::Word64) = putWord8 0xFe >> putWord64le (fromIntegral x) 

    get     = VarInt <$> (getWord8 >>= getValue) where 
        getValue size   | size  < 0xFD  = return $ fromIntegral size
                        | size == 0xFD  = fromIntegral <$> getWord16le 
                        | size == 0xFE  = fromIntegral <$> getWord32le
                        | otherwise     = fromIntegral <$> getWord64le 
    
