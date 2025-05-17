{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Filterload where 

import System.Random.MWC (withSystemRandom, asGenIO, uniform) 
import GHC.Generics 
import Data.Binary      (Binary(..)     )
import Data.Word        (Word8, Word32  )
import Data.ByteString  (ByteString     ) 
import Data.Bits        ((.|.),(.&.),shiftL,shiftR)
import Data.Array       (Array, listArray, elems, accum)

import Prelude hiding (filter) 

import Data.Hash.Murmur 
import GHC.Exts (fromList) 

import Bitcoin.Types 
import Bitcoin.VarList (VarList)

data Filterload = Filterload 
    { filter        :: VarList Word8
    , nHashFuncs    :: Word32le
    , nTweak        :: Word32le
    , nFlags        :: Word8
    }       deriving (Show, Generic) 

instance Binary Filterload 

instance Message Filterload where 
    commandName = "filterload"



filterload :: Word32 -> Word32 -> [ByteString] -> IO Filterload
filterload size nHashFuncs queries = do 
    nTweak <- withSystemRandom $ asGenIO uniform
    let seeds       = map (\n -> n* 0xFBA4C795 + nTweak) [0 .. nHashFuncs - 1] 
        bits        = do 
            seed        <- seeds 
            query       <- queries 
            return $ murmur3 seed query `mod` size 
        bitArray    = accum
            (\e a -> e .|. shiftL 1 (fromIntegral $ 7 .&. a))
            (listArray (0, size `div` 8 - 1)(repeat 0))
            (map (\i -> (shiftR i 3, i)) bits)
    return $ Filterload(fromList $ elems bitArray)(Word32le nHashFuncs)(Word32le nTweak)1



