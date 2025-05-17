{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE OverloadedStrings #-} 


module Bitcoin.MerkleBlock where 

import Data.Binary
import GHC.Generics
import Data.Binary (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Types
import Bitcoin.VarList (VarList)
import Bitcoin.Crypto (hash256)


data MerkleBlock = MerkleBlock
    { version           :: Int32le
    , prevBlock         :: Chars 32
    , merkleRoot        :: Chars 32
    , timestamp         :: Word32le 
    , bits              :: Word32le
    , nonce             :: Word32le
    , totalTransactions :: Word32le 
    , hashes            :: VarList (Chars 32)
    , flags             :: VarList Word8
    }           deriving (Show,Generic) 

instance Binary MerkleBlock

instance Message MerkleBlock where 
    commandName = "merkelblock"


blockHash   ::  MerkleBlock -> Chars 32 
blockHash   =   toChars . hash256 . BS.take 80 . BL.toStrict . encode   
