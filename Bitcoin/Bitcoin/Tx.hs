{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-} 

module Bitcoin.Tx where 

import Bitcoin.Types
import Bitcoin.VarStr
import Bitcoin.VarList
import Bitcoin.Crypto
import Bitcoin.Script

import Data.List (find) 
import GHC.Generics
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL

type TxId = Chars 32 


data Output = Output 
    { hash              :: TxId
    , index             :: Word32le
    } deriving (Show, Eq,  Generic) 

instance Binary Output

data TxIn = TxIn 
    { previousOutput    :: Output
    , signatureScript   :: VarStr
    , sequence          :: Word32le
    } deriving (Show, Generic) 

instance Binary TxIn

data TxOut = TxOut
    { value             :: Int64le
    , pkScript          :: VarStr
    } deriving (Show, Generic) 

instance Binary TxOut

data Tx = Tx 
    { version   :: Int32le
    , txIn      :: VarList TxIn
    , txOut     :: VarList TxOut
    , lockTime  :: Word32le
    } deriving (Show, Generic)

instance Binary Tx

instance Message Tx where 
    commandName = "tx"

txId :: Tx -> TxId 
txId = toChars . hash256 . BL.toStrict . encode




findP2PKHIndex :: ByteString -> Tx -> Maybe Word32le
findP2PKHIndex pkh tx   = 
    let scripts             = map (string . pkScript) . elems $ txOut tx
        p2pkhHeader         = BS.concat [op_DUP, op_HASH160] 
        isP2PKH             = BS.isPrefixOf p2pkhHeader
        getPKH              = BS.take 20 . BS.drop (BS.length p2pkhHeader + 1) in 
    fromIntegral . snd <$> (find((pkh==).getPKH.fst).filter(isP2PKH.fst) $ zip scripts [0..])

hasOutput :: Output -> Tx -> Bool
hasOutput op tx = elem op . map previousOutput . elems $ txIn tx


valueAt :: Word32le -> Tx -> Int64le 
valueAt index tx = value $ (elems $ txOut tx) !! (fromIntegral index)
