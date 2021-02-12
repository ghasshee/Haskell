{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE OverloadedStrings #-} 


module Bitcoin.Version where 

import GHC.Generics 
import Data.Binary 


import Bitcoin.Types
import Bitcoin.NetAddr 
import Bitcoin.VarStr 


data Version = Version 
    { version       :: Int32le 
    , services      :: Word64le
    , timestamp     :: Int64le 
    , addrRecv      :: NetAddr
    , addFrom       :: NetAddr
    , nonce         :: Word64le 
    , userAgent     :: VarStr
    , startHeight   :: Int32le 
    , relay         :: Bool     } 
        deriving (Show, Generic) 

instance Binary Version 

instance Message Version where 
    commandName = "version" 
