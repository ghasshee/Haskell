{-# LANGUAGE DataKinds          #-} 
{-# LANGUAGE DeriveGeneric      #-} 
{-# LANGUAGE OverloadedStrings  #-} 


module Bitcoin.GetBlocks where 

import GHC.Generics     (Generic    )
import Data.Binary      (Binary(..) )
import Data.Maybe       (fromJust   )
import qualified Data.Vector.Sized as Vector

import Bitcoin.Types
import Bitcoin.VarList  (VarList    )

data GetBlocks = GetBlocks 
    { version                   :: Int32le
    , blockLocatorHashes        :: VarList (Chars 32)
    , hashStop                  :: Chars 32     
    }           deriving (Show, Generic)


instance Binary GetBlocks 

instance Message GetBlocks where 
    commandName = "getblocks" 

-- The Hash registered when requiring all blocks 
zeroHash    ::  Chars 32 
zeroHash    =   Chars . fromJust . Vector.fromList $ replicate 32 0 




