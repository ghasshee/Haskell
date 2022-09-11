{-# LANGUAGE BinaryLiterals         #-}
{-# LANGUAGE OverloadedStrings      #-} 

module DB where 

import Data.Default     (   def     ) 

import Database.RocksDB  (  Compression (..), DB, compression,
                            createIfMissing, defaultOptions, get, open, put ) 

import Data.ByteString 

initializeDB path =
    open 
        path 
        defaultOptions
        {createIfMissing = True, compression = NoCompression} 


putget :: IO (Maybe ByteString)
putget = do 
    db <- initializeDB "./hoge"
    put db def "hoge" "foo" 
    get db def "hoge"
