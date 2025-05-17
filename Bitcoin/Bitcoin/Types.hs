{-# LANGUAGE DataKinds                  #-} 
{-# LANGUAGE RankNTypes                 #-} 
{-# LANGUAGE AllowAmbiguousTypes        #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE TypeApplications           #-} 
{-# LANGUAGE ScopedTypeVariables        #-}

module Bitcoin.Types where 


import Data.Int
import Data.Word 
import Data.Binary 
import Data.Binary.Get
import Data.Binary.Put

import Data.Vector.Sized (Vector) 
import qualified Data.Vector.Sized as Vector 
import GHC.TypeLits   

import Data.Proxy 
import Data.Maybe
import Control.Monad 

import Data.ByteString (ByteString)  
import qualified Data.ByteString as BS 

newtype Word32le = Word32le Word32 
    deriving (Show, Eq,Ord, Enum, Bounded, Num, Real, Integral) 

instance Binary Word32le where
    put (Word32le x) = putWord32le x 
    get = Word32le <$> getWord32le 

newtype Word64le = Word64le Word64 
    deriving (Show, Eq,Ord, Enum, Bounded, Num, Real, Integral) 

instance Binary Word64le where
    put (Word64le x) = putWord64le x 
    get = Word64le <$> getWord64le 



newtype Int32le = Int32le Int32 
    deriving (Show, Eq,Ord, Enum, Bounded, Num, Real, Integral) 

instance Binary Int32le where
    put (Int32le x) = putInt32le x 
    get = Int32le <$> getInt32le 

newtype Int64le = Int64le Int64 
    deriving (Show, Eq,Ord, Enum, Bounded, Num, Real, Integral) 

instance Binary Int64le where
    put (Int64le x) = putInt64le x 
    get = Int64le <$> getInt64le 



--------------

newtype Chars n = Chars (Vector n Word8) 
    deriving (Show, Eq, Ord) 

instance forall n. KnownNat n => Binary (Chars n) where 
    put (Chars x)   = mapM_ put $ Vector.toList x 
    get             = (Chars . fromJust . Vector.fromList) <$> (replicateM (fromInteger $ natVal (Proxy @n)) get)


toChars     :: forall n. KnownNat n => ByteString -> Chars n 
toChars bs  =  Chars . fromJust . Vector.fromList . take 
                    (fromInteger $ natVal (Proxy @n)) $ (BS.unpack bs ++ repeat 0x00) 

toByteString :: Chars n -> ByteString 
toByteString (Chars cs) = BS.pack $ Vector.toList cs 



--------------


class Message a where 
    commandName :: ByteString 
