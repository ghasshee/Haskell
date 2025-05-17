module Bitcoin.Crypto where 


import Crypto.Hash 
import Data.ByteString  (ByteString) 
import Data.ByteArray   (convert) 


sha256          ::  ByteString -> ByteString 
sha256 bs       =   convert (hash bs :: Digest SHA256) 

hash256         ::  ByteString -> ByteString 
hash256         =   sha256 . sha256 

ripemd160       ::  ByteString -> ByteString 
ripemd160 bs    =   convert (hash bs :: Digest RIPEMD160) 

hash160         ::  ByteString -> ByteString 
hash160         =   ripemd160 . sha256

blake           ::  ByteString -> ByteString 
blake   bs      =   convert (hash bs :: Digest Blake2s_256)

