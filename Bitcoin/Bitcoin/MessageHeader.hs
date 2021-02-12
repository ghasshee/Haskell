{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE DeriveGeneric #-} 

module Bitcoin.MessageHeader where


import Data.Binary 
import GHC.Generics 

import Bitcoin.Types



data MessageHeader = MessageHeader 
    { magic         :: Word32le 
    , commandName   :: Chars 12 
    , payloadSize   :: Word32le 
    , checksum      :: Chars 4 
    }       deriving (Show, Generic) 

instance Binary MessageHeader   
