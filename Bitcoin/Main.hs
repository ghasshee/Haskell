{-# LANGUAGE OverloadedStrings #-}


import System.Environment (getArgs)
import Data.ByteString
import Bitcoin.Protocol
import Control.Monad
import Bitcoin.Wallet

import Prelude hiding (length)
import qualified Prelude as P
import GHC.List hiding (head) 
import qualified GHC.List as L 

{--
main :: IO ()
main = withBitcoinConnection $ \(sock, _) -> do
    forever $ dispatch sock =<< recvMessageHeader sock 
        where 
            dispatch sock (name, size) = 
                if size >0 
                    then recvMessage sock size :: IO ByteString
                    else pure "" 
--}
main :: IO ()
main = do
    args <- getArgs
    if P.length args == 0 
        then P.putStrLn "Please give subcommand wallet/send/balance"
        else case L.head args of 
            "wallet"    -> showWallet
            "hello"     -> withBitcoinConnection hello

hello (sock,_) = do
    forever $ recvMessageHeader sock >>= dispatch sock
        where 
            dispatch sock (name,size) = 
                if size<=0 
                    then pure ""
                    else recvMessage sock size :: IO ByteString
                

