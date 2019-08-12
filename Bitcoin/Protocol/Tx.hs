{-# LANGUAGE 
      DeriveGeneric,
      DataKinds,
      OverloadedStrings #-}

module Bitcoin.Protocol.Tx where

import Prelude hiding (sequence)
import Data.List (find)
import GHC.Generics (Generic)

import Data.Binary (Binary(..), encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Hash (hash256)
import Bitcoin.Types (Message(..), Chars,toChars,Word32le,Int32le,Int64le)
import Bitcoin.Protocol.VarStr (VarStr)
import qualified Bitcoin.Protocol.VarStr as VarStr
import Bitcoin.Protocol.VarList as VarList
import Bitcoin.Script (op_dup, op_hash160) 



-- | Transaction ID
type TxId = Chars 32

-- | reference to TxOut
data OutPoint = OutPoint
    { hash  :: TxId
    , index :: Word32le } deriving (Show, Generic)

instance Binary OutPoint 


data TxIn   = TxIn
    { previousOutput    :: OutPoint
    , signatureScript   :: VarStr
    , sequence          :: Word32le } deriving (Show, Generic)

instance Binary TxIn

data TxOut  = TxOut
    { value             :: Int64le
    , pkScript          :: VarStr } deriving (Show, Generic)

instance Binary TxOut


-- | omitted  "witness" 
-- TODO : witness
data Tx = Tx
    { version           :: Int32le
    , txIn              :: VarList TxIn
    , txOut             :: VarList TxOut
    , lockTime          :: Word32le } deriving (Show, Generic)

instance Binary Tx 

instance Message Tx where
    commandName = "tx"

txId    ::  Tx -> TxId
txId    =   toChars . hash256 . BL.toStrict . encode

{-- 
findP2PkhIndex          ::  ByteString -> Tx -> Maybe word32le
findP2PkhIndex  pkh tx  =   
    let scripts     =   map (VarStr.string . pkScript). VarList.elems $ txOut tx
        p2pkhHeader =   BS.concat [op_dup, op_hash160]
        itIsP2Pkh   =   BS.isPrefixOf p2pkhHeader
        getPkh      =   BS.take 20 . BS.drop(BS.length p2pkhHeader + 1) in
    (fromIntegral . snd) <$> (find((pkh==).getPkh.fst).filter (itIsP2Pkh . fst) $ zip scripts [0..])
--} 

