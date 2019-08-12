{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bitcoin.Protocol.GetData where 

import GHC.Generics (Generic(..))
import Data.Binary (Binary(..)) 
import Bitcoin.Types (Message(..))
import Bitcoin.Protocol.VarList (VarList)
import Bitcoin.Protocol.Inv (Inventory)

newtype GetData = GetData (VarList Inventory)
    deriving (Show, Generic)

instance Binary GetData

instance Message GetData where 
    commandName = "getdata" 


