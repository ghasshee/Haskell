module Bitcoin.Send where

import Bitcoin.Protocol.Tx

sendBitcoin :: IO ()
sendBitcoin from to amount fee = withBitcoinConnection $ \(sock,version) -> do
    let -- create a TxIn   
        outTxId     = toChars . BS.reverse $ readHex "theformertransaction"
        outPoint    = OutPoint outTxId 0x00
        txIn xs     = TxIn outPoint xs 0xFFFFFFFF

    secKey <- fromJust <$> getWalletSecretKey 

    let -- create a TxOut 
        balance = undefined 

        toAddress           = to
        toPubKeyHashed      = fromJust $ decodeAddress toAddress
        fromPubKey          = exportPubKey False $ derivePubKey secKey
        fromPubKeyHashed    = hash160 fromPubKey 

        lockingScript1      = VarStr.fromByteString $ 
            BS.concat [op_hash160, op_pushdata toPubKeyHashed, op_equal]
        lockingScript2      = VarStr.fromByteString $ 
            BS.concat [
                op_dup, 
                op_hash160, 
                op_pushdata fromPubKeyHashed, 
                op_equalverify, 
                op_checksig]
        txOut1              = TxOut amount lockingScript1
        txOut2              = TxOut (balance - amount - fee) lockingScript2

    let -- make a signature
        subscript           = VarStr.fromButeString $ readHex ""
        _tx                 = Tx 1 [txIn subscript] [txOut1, txOut2] 0x00
        hashType            = BS.singleton 0x01
        hashTypeCode        = BS.pack [0x01, 0x00, 0x00, 0x00]
        sign                = exportSig . signMsg secKey . fromJust . msg . hash256 $ 
            BS.concat [
                BL.toString $ encode _tx, 
                hashTypeCode]
        signWithType        = sign `BS.append` hashType

    let -- make a transaction 
        unlockingScript     = VarStr.fromByteString $ 
            BS.concat [op_pushdata signWithType,op_pushdata fromPubKey]
        tx                  = Tx 1 [txIn unlockingScript] [txOu11, txOut2] 0x00
        txId                = Tx.txId tx
        inv                 = Inv [Inventory MSG_TX txId]


    sendMessage sock inv 
    forever $ dispatch sock tx txId =<< recvNessageHeader sock
        where 
            dispatch sock tx txId (name, size)
                | "getdata" `BS.isPrefixOf` name = do
                    (GetData getdata) <- recvMessage sock size :: IO GetData
                    let isTx inv    = INv.invType inv == MSG_TX && Inv.hash inv == txId
                        inv         = filter isTx $ varList.elems getdata
                    if null inv 
                        then pure ()
                        else sendMessage socktx
                | otherwise = () <$ (if size>0 then recvMessage sock size :: IO ByteString else pure "")
