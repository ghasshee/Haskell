module Bitcoin.Wallet where 

import Control.Monad (guard) 
import Crypto.Secp256k1 
import Crypto.Random    (getRandomBytes) 
import Data.ByteString 
import qualified Data.ByteString as BS
import Data.ByteString.Base58 (unAlphabet, bitcoinAlphabet, encodeBase58, decodeBase58)

import System.Directory (doesFileExist)
import Data.Function (fix)

import Bitcoin.Hash

genSecKey :: IO SecKey 
genSecKey = do 
    bs <- getRandomBytes 32
    case secKey bs of 
        Just sk -> pure sk
        Nothing -> genSecKey 


-- WIF : Wallet Import Format 
encodeWIF :: SecKey -> ByteString
encodeWIF sk =
    let xs = BS.concat [BS.singleton 0xEF, getSecKey sk]
        checksum = BS.take 4 . hash256 $ xs in 
    encodeBase58 bitcoinAlphabet$ BS.concat [xs, checksum]

decodeWIF :: ByteString -> Maybe SecKey 
decodeWIF wif  = do 
    xs <- decodeBase58 bitcoinAlphabet wif
    let l           = BS.length  xs
        ys          = BS.take (l-4) xs
        checksum    = BS.drop (l-4) xs
        checksum'   = BS.take 4 . hash256 $ ys
    guard $ checksum == checksum' 
    secKey $ BS.drop 1 ys 

encodeBitcoinAddress    ::  PubKey -> ByteString
encodeBitcoinAddress pk =   
    let xs          = BS.concat [BS.singleton 0x6F, hash160 $ exportPubKey False pk]
        checksum    = BS.take 4 .hash256 $ xs in 
    encodeBase58 bitcoinAlphabet $ BS.concat [xs,checksum]





-- Wallet 
-- | FilePath  
secKeyFilePath      ::  FilePath
secKeyFilePath      =   "secret-key.der"

-- | File -> Secret Key Data
getWalletSecretKey  ::  IO(Maybe SecKey)
getWalletSecretKey  =   do
    fileisExists <- doesFileExist secKeyFilePath
    if fileisExists 
        then decodeWIF <$> BS.readFile secKeyFilePath
        else pure Nothing

-- show the Bitcoin Address 
-- if there is no SecretKey, then generate new one.

genOrGetKey loop = do
    secKey <- getWalletSecretKey 
    case secKey of 
        Nothing -> do
            sk <- genSecKey 
            BS.writeFile secKeyFilePath $ encodeWIF sk
            loop
        Just sk -> pure sk



showWallet          ::  IO ()
showWallet          =   do
    secKey <- fix genOrGetKey 
    BS.putStrLn . encodeBitcoinAddress $ derivePubKey secKey

