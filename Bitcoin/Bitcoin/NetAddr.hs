{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE DeriveGeneric #-} 


module Bitcoin.NetAddr where 

import GHC.Generics 
import Data.Binary 

import Bitcoin.Types 


data NetAddr = NetAddr 
    { services      :: Word64le 
    , ip            :: Chars 16 
    , port          :: Word16 
    }           deriving (Show, Generic) 

instance Binary NetAddr 


