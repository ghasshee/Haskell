{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE OverloadedLists #-} 


module Balance where

import Control.Concurrent (forkIO, threadDelay) 
import Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, modifyTVar)
import Control.Monad.STM (atomically)
import Control.Monad (forever, when, guard) 

import Data.Function (on, fix) 
import Data.List (maximumBy) 

import GHC.Exts (fromList) 

 

import Data.Maybe (fromJust, isJust, isNothing) 
import qualified Data.Map as Map 
import qualified Data.ByteString as BS
import Data.ByteString (ByteString) 

import Crypto.Secp256k1 (exportPubKey, derivePubKey)

import Bitcoin.Crypto (hash160)
import Bitcoin.Protocol
import qualified Bitcoin.Version as Version
import Bitcoin.Filterload hiding (filter) 
import Bitcoin.Types (toChars)
import Bitcoin.MerkleBlock (MerkleBlock(..), blockHash, timestamp) 
import Bitcoin.MerkleTree
import Bitcoin.GetBlocks 
import Bitcoin.GetData
import Bitcoin.Inv (Inv(..), Inventory(..), InvType(..), invType, hash)  
import Bitcoin.Reject
import Bitcoin.Tx hiding (hash, sequence)  
import Bitcoin.VarList (VarList(VarList))
import qualified Bitcoin.VarList as VarList
import Send
import Wallet 

showBalance     ::  IO ()
showBalance     =   connectBitcoinNetwork $ \(sock, version) -> do 
    
    secKey              <-  fromJust <$> getWalletSecretKey

    print $ "debug: got secKey"

    let fromPK          =   exportPubKey False $ derivePubKey secKey
        fromPKHash      =   hash160 fromPK

        -- start sync from Block Height : 1902500
        startBlockHash  =   toChars . BS.reverse $ readHex
                    "000000004b107860533d8978f904b348b2ceeda15706618bb0fe70cef4050673"
        -- 1255000: "000000006ba819bcaa50e6fb2fb89deb4ff894068cbaf4b4b9b7e89f24ccdc2f"
        leftBlocks      =   fromIntegral $ Version.startHeight version - 1902500

    print $ "debug: left blocks = " ++ show leftBlocks 

    sendMessage sock =<< filterload 1024 10 [fromPKHash] 

    -- fork thread and record blocks 
    blockMapTVar        <- newTVarIO Map.empty 
    txMapTVar           <- newTVarIO Map.empty 
    forkIO . forever $ dispatch sock(blockMapTVar,txMapTVar)=<< recvMessageHeader sock 

    print "debug: fork done."

    let pr = do 
            blockMap <- readTVarIO blockMapTVar 
            let m = Map.size blockMap 
            print $ show m ++ " blocks downloaded." 
            threadDelay 3000000
            pr 
    
    

    -- Get 500 Blocks at a time, lasting until the goal. 
    let loop n          = do 
            threadDelay 500000 -- sleep 0.1 s
            blockMap            <- readTVarIO blockMapTVar
            let m = Map.size blockMap 
            if m  >= leftBlocks - 1000
                then return blockMap 
                else    
                    if m  >= n 
                        then do 
                                let latestBH = blockHash . maximumBy(compare `on` timestamp)$
                                                    Map.elems blockMap
                                sendMessage sock $ GetBlocks 70015 [latestBH] zeroHash
                                -- 70015 : Bitcoin Protocol Version (2018-) 
                                loop (n+500) 
                        else do  
                                loop n

    
    sendMessage sock $ GetBlocks 70015 [startBlockHash] zeroHash
    
    print "debug: getblock sended. "

    forkIO . forever $ pr

    blockMap <- loop 500

    print "debug: sync done."

    print $ Map.size blockMap 


    -- TODO : extract UTXO and calculate Balance

    txMap <- readTVarIO txMapTVar
    
    let txs         = concat . fromJust . sequence . map validate $ Map.elems blockMap 
        unknowns    = filter (isNothing . flip Map.lookup txMap) txs
    
    sendMessage sock (GetData . fromList $ map (Inventory MSG_TX) unknowns)

    txMap <- fix $ \loop -> do 
        threadDelay 100000
        txMap           <- readTVarIO txMapTVar 
        if and $ map (isJust . flip Map.lookup txMap) unknowns
            then return txMap
            else loop

    let txs     = Map.elems txMap 
        myTxs   = do 
            tx <- txs 
            case findP2PKHIndex fromPKHash tx of 
                Just index  -> [(tx, index)] 
                Nothing     -> []
        utxo    = do 
            (tx, index) <- myTxs 
            let op      = Output (txId tx) index 
            guard $ and (map (not . (hasOutput op)) txs) 
            return (tx, index) 
        balance = sum $ map (\(tx, index) -> valueAt index tx) utxo

    putStrLn $ "Balance: " ++ show balance 




        where 
            dispatch sock (blockMapTVar, txMapTVar) (name,size)
                | "inv"         `BS.isPrefixOf` name    = do
                    threadDelay 100000
                    (Inv inv)   <- recvMessage sock size :: IO Inv 

                    let filterBlock x   =   if invType x == MSG_BLOCK
                                                then Inventory MSG_FILTERED_BLOCK(hash x)
                                                else x
                        inv'            =   map filterBlock $ VarList.elems inv
                        hashes          =   map hash inv'

                    sendMessage sock (GetData $ fromList inv')
                | "merkleblock" `BS.isPrefixOf` name    = do
                    block       <- recvMessage sock size :: IO MerkleBlock
                    atomically . modifyTVar blockMapTVar $ Map.insert(blockHash block)block
                | "tx"          `BS.isPrefixOf` name    = do
                    tx          <- recvMessage sock size :: IO Tx
                    atomically . modifyTVar txMapTVar $ Map.insert(txId tx) tx
                | "reject"      `BS.isPrefixOf` name    = do
                    threadDelay 100000
                    reject      <- recvMessage sock size :: IO Reject
                    print reject
                | otherwise                             = do
                    threadDelay 100000
                    if size > 0 
                        then    () <$ (recvMessage sock size :: IO ByteString)
                        else    return () 



