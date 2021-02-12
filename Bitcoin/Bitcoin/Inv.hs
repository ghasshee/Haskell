{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-} 

module Bitcoin.Inv where

import GHC.Generics     (Generic    )
import Data.Binary      (Binary(..) )
import Data.Binary.Put  (putWord32le)
import Data.Binary.Get  (getWord32le)

import Bitcoin.VarList  (VarList(VarList)   )
import Bitcoin.Types    (Message(..),Chars  )


newtype Inv = Inv (VarList Inventory)
    deriving (Show, Generic)

instance Binary Inv 

instance Message Inv where 
    commandName = "inv"


data Inventory = Inventory 
    { invType   :: InvType
    , hash      :: Chars 32
    } deriving (Show, Generic)

instance Binary Inventory


data InvType    = ERROR
                | MSG_TX
                | MSG_BLOCK
                | MSG_FILTERED_BLOCK
                | MSG_CMPCT_BLOCK
                deriving (Show, Eq) 

instance Binary InvType where
    put ERROR               = putWord32le 0
    put MSG_TX              = putWord32le 1
    put MSG_BLOCK           = putWord32le 2
    put MSG_FILTERED_BLOCK  = putWord32le 3
    put MSG_CMPCT_BLOCK     = putWord32le 4

    get = do 
        _type <- getWord32le 
        pure $ case _type of 
            0 -> ERROR
            1 -> MSG_TX
            2 -> MSG_BLOCK
            3 -> MSG_FILTERED_BLOCK
            4 -> MSG_CMPCT_BLOCK

