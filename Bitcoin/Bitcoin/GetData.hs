{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-} 


module Bitcoin.GetData where 

import GHC.Generics     (Generic    )
import Data.Binary      (Binary(..) )

import Bitcoin.VarList  (VarList    )
import Bitcoin.Inv      (Inventory  )
import Bitcoin.Types    (Message(..))

newtype GetData = GetData (VarList Inventory) 
    deriving (Show,Generic) 

instance Binary GetData 

instance Message GetData where
    commandName = "getdata" 
