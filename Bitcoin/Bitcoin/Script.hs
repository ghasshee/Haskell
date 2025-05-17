


module Bitcoin.Script where 

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS 

op_PUSHDATA     ::  ByteString -> ByteString 
op_PUSHDATA bs  =   let size = BS.singleton . fromIntegral $ BS.length bs in 
                    size `BS.append` bs 


op_DUP          ::  ByteString 
op_DUP          =   BS.singleton 0x76 -- x , xx , Dupricates the top stack item 

op_EQUAL        =   BS.singleton 0x87

op_EQUALVERIFY  =   BS.singleton 0x88
op_HASH160      =   BS.singleton 0xA9
op_CHECKSIG     =   BS.singleton 0xAC




