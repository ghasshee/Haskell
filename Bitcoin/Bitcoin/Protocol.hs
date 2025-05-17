{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TypeApplications #-} 
{-# LANGUAGE UnicodeSyntax #-} 


module Bitcoin.Protocol where 



import Network.Socket 
import Network.Socket.ByteString 
import Network.BSD (getHostByName, hostAddress) 


import Data.Time.Clock.POSIX (getPOSIXTime) 

import Data.Char (chr) 
import Data.Function (fix) 
import Data.Binary 
import Data.ByteString (ByteString)
import Data.ByteArray.Encoding (Base(Base16), convertFromBase, convertToBase)
import qualified Data.ByteString        as BS 
import qualified Data.ByteString.Lazy   as BL 
import qualified Data.ByteString.Char8  as BC 

import Control.Applicative 
import Control.Exception 

import Bitcoin.Types 
import Bitcoin.MessageHeader (MessageHeader(MessageHeader))
import qualified Bitcoin.MessageHeader as MessageHeader 
import Bitcoin.Crypto 
import Bitcoin.Version 
import Bitcoin.Verack 
import Bitcoin.Reject
import Bitcoin.NetAddr 
import Bitcoin.VarStr (VarStr(VarStr))
import Bitcoin.VarInt


-------  Receive  -------------- 
recvMessage             :: Binary msg => Socket -> Int -> IO msg 
recvMessage sock size   = decode . BL.fromStrict <$> recvAll sock size 

recvMessageHeader :: Socket -> IO (ByteString, Int) 
recvMessageHeader sock = do 
    mh <- recvMessage sock 24 :: IO MessageHeader 
    let name = toByteString $ MessageHeader.commandName mh 
        size = fromIntegral $ MessageHeader.payloadSize mh 
    putStrLn $ "Recv: " ++ BC.unpack name ++ " " ++ show size 
    return (name,size) 

recvAll :: Socket -> Int -> IO ByteString 
recvAll sock size = go [] sock size where 
    go received _ 0         = pure . BS.concat $ reverse received 
    go received sock size   = do 
        bs <- recv sock size 
        go (bs:received) sock (size - BS.length bs) 


--------   Send   -------------- 
sendMessage :: ∀ msg . (Binary msg, Message msg) => Socket -> msg -> IO () 
sendMessage sock msg = do 
    let header  = createMessageHeader msg 
        payload = encode msg 
        size    = BL.length payload 
    send sock (BL.toStrict $ BL.concat [encode header, payload] )
    putStrLn $ "Send: " ++ (BC.unpack $ commandName @msg) ++ " " ++ show size 

createMessageHeader :: ∀ msg. (Binary msg, Message msg) => msg -> MessageHeader 
createMessageHeader message = 
    let payload = BL.toStrict $ encode message  in 
    MessageHeader 
        0x0709110B      -- Testnet magic value 
        -- 0xD9B4BEF9   -- Mainnet magic value 
        (toChars $ commandName @msg)
        (fromIntegral $ BS.length payload)
        (toChars $ hash256 payload) 



----- Handshake Protocol ------


-----           Hex         -> ByteString 
readHex     :: ByteString   -> ByteString 
readHex     = either error id . convertFromBase Base16 

toHex       :: ByteString   -> ByteString 
toHex       = convertToBase Base16 


-- convertFromBase :: ByteArrayAccess b_in, ByteArray b_out => 
--                      Base -> b_in -> Either String b_out  



-- bracket :: IO a  -> (a -> IO b)  -> (a -> IO c) -> IO c 
-- bracket = \init  -> \err_final   ->  \between   -> return

connectBitcoinNetwork :: ((Socket, Version) -> IO a) -> IO a
connectBitcoinNetwork between = bracket first (close . fst) between where 
    -- init
    first   = do 
        host <- hostAddress <$> getHostByName "testnet-seed.bitcoin.jonasschnelli.ch" 
        sock <- socket AF_INET Stream defaultProtocol
        connect sock (SockAddrInet 18333 host) 

        -- send version unixTime
        unixTime <- getPOSIXTime 
        let ip        = readHex $ BS.concat ["00000000","00000000","0000FFFF","7F000001"] 
                        --127.0.0.1
            addr      = NetAddr 1 (toChars ip) 8333
            userAgent = VarStr  0 BS.empty
            version   = Version 70015 1 (round unixTime) addr addr 0 userAgent 0 False
        sendMessage sock version 

        -- receive VERSION && send VERACK
        version <- handshake sock Nothing False 
        return  (sock, version) 

    
    -- loop untl handshake 
    handshake sock version verack           = do 
        case (version, verack) of 
            (Just version, True)    -> return version 
            _                       -> do 
                (name,size)             <- recvMessageHeader sock 
                (version',verack')      <- dispatch sock name size 
                handshake sock (version <|> version') (verack || verack') 


    dispatch sock name size 
        | "version" `BS.isPrefixOf` name    = do 
            version     <- recvMessage sock size :: IO Version
            sendMessage sock Verack
            pure (Just version,   False) 
        | "verack" `BS.isPrefixOf` name     = return (Nothing, True) 
        | otherwise                         = do 
            recvMessage sock size :: IO ByteString 
            return (Nothing,False) 



