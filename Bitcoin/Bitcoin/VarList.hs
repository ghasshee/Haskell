{-# LANGUAGE TypeFamilies #-}

module Bitcoin.VarList where 

import Prelude hiding (length) 
import qualified Prelude as P 

import GHC.Exts (IsList(..))

import Control.Monad
import Data.Binary (Binary(..))

import Bitcoin.VarInt

data VarList a = VarList 
    { length    :: VarInt 
    , elems     :: [a]
    } deriving Show

instance IsList (VarList a) where 
    type Item (VarList a) = a 
    fromList xs     = VarList (fromIntegral $ P.length xs) xs
    toList          = elems

instance Binary a => Binary (VarList a) where 
    put x = do 
        put $ length x
        mapM_ put $ elems x
    get = do 
        size    <- get
        es      <- replicateM (fromIntegral size) get
        return (VarList size es)


