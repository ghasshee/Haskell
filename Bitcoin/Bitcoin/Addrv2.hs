{-# LANGUAGE DataKinds          #-} 
{-# LANGUAGE OverloadedStrings  #-} 
{-# LANGUAGE DeriveGeneric      #-}


module Bitcoin.Addrv2 where 

import Data.Binary
import GHC.Generics
import Data.Vector

import Bitcoin.Types
import Bitcoin.VarList

data Addrv2 = Addrv2 
    { time          :: Chars 32
    , services      :: Word64le
    , networkID     :: Word8
    , addr          :: VarList (Chars 8)
    , port          :: Word16
    }       deriving (Show, Generic) 


instance Binary Addrv2 

instance Message Addrv2 where 
    commandName = "addrv2" 
