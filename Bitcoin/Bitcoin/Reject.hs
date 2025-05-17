{-# LANGUAGE OverloadedStrings #-} 

module Bitcoin.Reject where 

import Data.ByteString.Lazy as BL
import Data.Word
import Data.Binary (Binary(..))
import Data.Binary.Put (putLazyByteString) 
import Data.Binary.Get (getRemainingLazyByteString) 

import Bitcoin.Types 
import Bitcoin.VarStr


data Reject = Reject 
    { message   :: VarStr   -- Reject Type
    , ccode     :: Word8    -- Reject code
    , reason    :: VarStr   
    ,_data      :: BL.ByteString -- TX ID , blockheader, ..etc
    } deriving Show 


instance Binary Reject where 
    put x = do 
        put $ message x
        put $ ccode x 
        put $ reason x 
        putLazyByteString $ _data x

    get = Reject
        <$> get
        <*> get
        <*> get
        <*> getRemainingLazyByteString

instance Message Reject where 
    commandName = "reject" 



