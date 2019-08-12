{-# LANGUAGE  DeriveGeneric
            , OverloadedStrings #-}

module Bitcoin.Protocol.Version where


import Data.Binary (Binary(..))
import GHC.Generics (Generic(..))

import Bitcoin.Types
import Bitcoin.Protocol.VarStr
import Bitcoin.Protocol.NetAddr


data Version = Version 
    { version       :: Int32le
    , services      :: Word64le
    , timestamp     :: Int64le
    , addrRecv      :: NetAddr
    , addrFrom      :: NetAddr
    , nonce         :: Word64le
    , userAgent     :: VarStr
    , startHeight   :: Int32le
    , relay         :: Bool
    } deriving (Show, Generic) 

instance Binary Version 

instance Message Version where
    commandName     = "version" 
