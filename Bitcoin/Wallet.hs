module Wallet where 


import Crypto.Secp256k1
import Crypto.Random (getRandomBytes) 


import Control.Monad (guard) 
import Data.ByteString (ByteString) 
import qualified Data.ByteString as BS
import Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58, decodeBase58)

import Data.Function (fix)
import System.Directory (doesFileExist)

import Bitcoin.Crypto (hash160, hash256)


-- generate Secret key
genSecKey :: IO SecKey 
genSecKey = do 
    bs      <- getRandomBytes 32 
    case secKey bs of 
        Just sk     -> pure sk 
        Nothing     -> genSecKey 


-- convert SecretKey -> WIF
encodeWIF :: SecKey -> ByteString 
encodeWIF sk = 
    let xs = BS.concat [BS.singleton 0xEF, getSecKey sk]
        checksum = BS.take 4 . hash256 $ xs in 
    encodeBase58 bitcoinAlphabet $ BS.concat [xs, checksum] 

decodeWIF :: ByteString -> Maybe SecKey 
decodeWIF wif = do 
    xs <- decodeBase58 bitcoinAlphabet wif
    let l           = BS.length xs
        ys          = BS.take(l-4) xs
        checksum    = BS.drop(l-4) xs
        checksum'   = BS.take 4 . hash256 $ ys 
    guard   $ checksum == checksum' 
    secKey  $ BS.drop 1 ys 


-- PubKey -> BitcoinAddress

encodeBitcoinAddress :: PubKey -> ByteString 
encodeBitcoinAddress pubkey = 
    let xs = BS.concat [BS.singleton 0x6F, hash160 $ exportPubKey False pubkey] 
        checksum = BS.take 4 .hash256 $ xs in 
    encodeBase58 bitcoinAlphabet $ BS.concat [xs, checksum] 





-- File Path for Store Secret Key 
secKeyFilePath :: FilePath
secKeyFilePath = "keys/secret-key.der" 


-- get Sec Key from the File 
getWalletSecretKey :: IO (Maybe SecKey)
getWalletSecretKey = do 
    isExists <- doesFileExist secKeyFilePath 
    if isExists
        then decodeWIF <$> BS.readFile secKeyFilePath
        else return Nothing 


-- show BTC Address 
-- if no key, generate new one.
showWallet :: IO() 
showWallet = do 
    secKey <- fix $ \loop -> do 
        secKey <- getWalletSecretKey 
        case secKey of 
            Nothing -> do 
                sk <- genSecKey 
                BS.writeFile secKeyFilePath $ encodeWIF sk 
                loop
            Just secKey -> pure secKey 
    BS.putStrLn . encodeBitcoinAddress $ derivePubKey secKey




-- decode Address to PubKey
decodeAddress :: ByteString -> Maybe ByteString
decodeAddress addr  = case decodeBase58 bitcoinAlphabet addr of
    Nothing             ->  Nothing 
    Just bs             ->  let l           = BS.length bs 
                                payload     = BS.take (l-4) bs 
                                checksum    = BS.drop (l-4) bs 
                                checksum'   = BS.take 4 . hash256 $ payload in 
                            if checksum == checksum' 
                                then Just $ BS.drop 1 payload
                                else Nothing

