{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bitcoin.Protocol.Verack where

import GHC.Generics (Generic(..))
import Data.Binary (Binary(..))
import Bitcoin.Types (Message(..))

data Verack         = Verack deriving (Show, Generic) 

instance Binary Verack

instance Message Verack where
    commandName     = "verack"


