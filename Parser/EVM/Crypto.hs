module Crypto where 

import Prelude 
import Data.Char 
import Data.ByteString hiding (map) 
import Crypto.Hash


sha1 :: ByteString -> Digest SHA1 
sha1 = hash 

md5 :: ByteString -> Digest MD5 
md5 = hash 

hex_sha3_512 :: ByteString -> String 
hex_sha3_512 bs = show ( hash bs :: Digest SHA3_512 ) 


hexSha3_512'  :: [Char] -> String 
hexSha3_512'  str = show (hash $ fromString  str :: Digest SHA3_512)


sha3_512 :: ByteString -> Digest SHA3_512 
sha3_512 = hash

sha3_384 :: ByteString -> Digest SHA3_384
sha3_384 = hash 

sha3_224 :: ByteString -> Digest SHA3_224 
sha3_224 = hash 
 
fromString :: String -> ByteString 
fromString = pack . map (fromInteger . toInteger . ord) 


keccak = undefined 
