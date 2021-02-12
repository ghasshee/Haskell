{-# LANGUAGE OverloadedStrings #-} 


module Main where

import Control.Monad
import System.Environment (getArgs) 

import Wallet 
import Send 
import Balance
import Bitcoin.Protocol
{--
main :: IO () 
main = connectBitcoinNetwork  $ \(sock, _) -> do 
    forever $ dispatch sock =<< recvMessageHeader sock where 
        dispatch sock (name,size) = 
            if size > 0 
                then recvMessage sock size :: IO ByteString 
                else pure ""
--}


main :: IO ()
main = do 
    args <- getArgs 
    if length args == 0 
        then putStrLn "Please give subcommand wallet/send/balance."
        else case head args of 
            "wallet"    -> showWallet
            "send"      -> send 
            "balance"   -> showBalance
