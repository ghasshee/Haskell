{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}


module Send where 

import Control.Monad (forever, when) 

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL 
import Data.Binary
import Data.Maybe

import Crypto.Secp256k1

import Bitcoin.Crypto
import Bitcoin.Tx 
import qualified Bitcoin.Tx as Tx 
import Bitcoin.Protocol
import Bitcoin.Types
import Bitcoin.VarStr
import Bitcoin.Inv 
import qualified Bitcoin.Inv as Inv 
import Bitcoin.GetData
import Bitcoin.Script
import qualified Bitcoin.VarList as VarList

import Wallet

send :: IO () 
send =  connectBitcoinNetwork $ \(sock,version) -> do 
    let 
        -- lastTx      = "df44b961e85859ea59991944ca69aa6de80340ee24d0f93d490f2b9cb78dd501"
        -- lastTx      = "18964e0905f84ae39c460b90ef0bae91ea2d4d396bee41ec75b49288fe15c3cc"
        lastTx      = "cda08833a480e4644964eff22388467e3523cb064718e22e893814c2487efca9"
        outTxId     = toChars . BS.reverse . readHex $ lastTx
        output      = Output outTxId 0x00       -- 0th output of the last tx is the sender
        txIn xs     = TxIn output xs 0xFFFFFFFF -- TxIn's sequence
    
    Just secKey <- getWalletSecretKey

    let 
        balance         = 1000000
        amount          =  200000
        fee             =  100000

        --toAddress     = "tb1qm5tfegjevj27yvvna9elym9lnzcf0zraxgl8z2"
        --toAddr          = "mtpgr1E24Dkws11ZghbzcfmNzm3z2BdCFR"
        toAddr          = "mupWFCy6tSeMXrg1zL4dM3VbXNMP5p39MA"
        toPKHash        = fromJust $ decodeAddress toAddr
        fromPK          = exportPubKey False $ derivePubKey secKey
        fromPKHash      = hash160 fromPK

        scr1            = BS.concat [op_HASH160, op_PUSHDATA toPKHash, op_EQUAL]
        scr2            = BS.concat [op_DUP,op_HASH160,op_PUSHDATA fromPKHash,op_EQUALVERIFY,op_CHECKSIG]
        script1         = fromByteString scr1 
        script2         = fromByteString scr2


        txOut1          = TxOut amount                   script1
        txOut2          = TxOut (balance - amount - fee) script2

    let 
        -- 0x76 : OP_DUP
        -- 0xA9 : OP_HASH160
        -- 0x14 : Bytes to Push 
        -- 20-bytes-PubKeyHash : Data to Push 
        -- 0x88 : OP_EQUALVERIFY
        -- 0xAC : OP_CHECKSIG
        -- lastScript      = "76a9149ce380ec3ee17d60710f7d61da7182efae14782788ac"
        -- lastScript      = "76a91491f439e98dd4e8810caeaed14c23df68fdcf14d088ac"
        lastScript      = "76a9146f7005308765f1e51f43ad4efc604cbe26fd97bf88ac"
        subscript       = fromByteString $ readHex lastScript
        tx'             = Tx 1 [txIn subscript] [txOut1, txOut2] 0x00
        hashType        = BS.singleton 0x01
        hashTypeCode    = BS.pack [0x01,0x00,0x00,0x00]
        tx_scr'         = BS.concat [BL.toStrict $ encode tx', hashTypeCode] 
        sign            = exportSig . signMsg secKey . fromJust . msg . hash256 $ tx_scr'
        signWithType    = sign `BS.append` hashType

    let 
        scr             = BS.concat [op_PUSHDATA signWithType, op_PUSHDATA fromPK]
        script          = fromByteString scr 
        tx              = Tx 1 [txIn script] [txOut1, txOut2] 0x00
        txId            = Tx.txId tx
        inv             = Inv [Inventory MSG_TX txId] 
    
    sendMessage sock inv 

    forever $ dispatch sock tx txId =<< recvMessageHeader sock 
        where 
            dispatch sock tx txId (name, size) 
                | "getdata" `BS.isPrefixOf` name    = do
                        GetData getdata <- recvMessage sock size    :: IO GetData
                        let isTx inv    = Inv.invType inv==MSG_TX && Inv.hash inv==txId
                            inv         = filter isTx $ VarList.elems getdata
                        if null inv 
                            then return ()
                            else sendMessage sock tx >> return () 
                | otherwise                         =  
                        () <$ (if size>0 
                                then recvMessage sock size  
                                else return "" :: IO ByteString )





