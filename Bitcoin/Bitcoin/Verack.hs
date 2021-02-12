{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE OverloadedStrings  #-} 

module Bitcoin.Verack where 

import GHC.Generics 
import Data.Binary 

import Bitcoin.Types 


data Verack = Verack 
    deriving (Show, Generic) 

instance Binary Verack 

instance Message Verack where
    commandName = "verack" 

