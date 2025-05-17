module Bitcoin.VarStr where 


import Prelude hiding (length) 
import Data.Binary 
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS  

import Bitcoin.VarInt 

data VarStr = VarStr 
    { length :: VarInt 
    , string :: BS.ByteString } 
        deriving Show 

instance Binary VarStr where 
    put x   = do 
        put             $ length x 
        putByteString   $ string x 
    get     = do 
        size    <- get 
        xs      <- getByteString (fromIntegral size) 
        return (VarStr size xs) 


fromByteString :: BS.ByteString -> VarStr
fromByteString bs = VarStr (fromIntegral $ BS.length bs) bs 



