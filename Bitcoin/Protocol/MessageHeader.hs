{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}


module Bitcoin.Protocol.MessageHeader where


import GHC.Generics (Generic(..))
import Data.Binary (Binary(..))
import Bitcoin.Types (Word32le, Chars) 

data MessageHeader = MessageHeader 
    { magic         :: Word32le
    , commandName    :: Chars 12
    , payloadSize   :: Word32le
    , checksum      :: Chars 4
    } deriving (Show, Generic) 

instance Binary MessageHeader 

