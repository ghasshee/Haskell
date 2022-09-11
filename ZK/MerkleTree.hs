module MerkleTree where


{-# LANGUAGE DataKinds          #-} 
{-# LANGUAGE OverloadedStrings  #-} 

import Data.List
import Data.ByteString
import qualified Data.Serialize as S 

import Crypto.Hash 

newtype MerkleHash a = MHash { getMHash :: ByteString } 
                        deriving (Show, Eq, Ord) 


data MerkleTree a   = MEmpty 
                    | MTree (MerkleNode a)

data MerkleNode a   = MBr (MerkleHash a) (MerkleNode a) (MerkleNode a)
                    | MLf (MerkleHash a) a 







-- 
-- module Bitcoin.MerkleTree where 
-- 
-- import Control.Monad.State (State)
-- import qualified Control.Monad.State as State
-- 
-- import Data.Word (Word8)
-- import Data.ByteString (ByteString) 
-- import qualified Data.ByteString as BS 
-- import Data.Bits 
-- 
-- import Bitcoin.Types (Word32le, Chars, toChars, toByteString, Int32le, Message(..))
-- import qualified Bitcoin.VarList as VarList 
-- import Bitcoin.MerkleBlock 
-- import Bitcoin.Crypto
-- 
-- 
-- unpack :: Word8 -> [Bool] 
-- unpack w = map (==1) $ map (\b -> (w `div` 2^b) `rem` 2) [0..7] 
-- 
-- 
-- calcTreeWidth :: Word32le -> Int -> Int 
-- calcTreeWidth nTxs height = (fromIntegral nTxs + (1 `shiftL` height) - 1) `shiftR` height
-- 
-- validate :: MerkleBlock -> Maybe [Chars 32] 
-- validate block = 
--     let hs                  = VarList.elems $ hashes block
--         fs                  = concatMap unpack . VarList.elems $ flags block
--         nTxs                = totalTransactions block
--         ctw                 = calcTreeWidth nTxs 
--         height              = ceiling . logBase 2 $ fromIntegral nTxs 
--         (root, (_,_,ms))    = State.runState (accum ctw height 0) (hs, fs, []) in 
--     if root == toByteString (merkleRoot block) 
--         then Just ms 
--         else Nothing 
--     where 
--         accum :: (Int->Int) -> Int -> Int -> State([Chars 32],[Bool],[Chars 32])ByteString 
--         accum ctw height pos    = do
--             (hl,fl,ms) <- State.get
--             let extract_fail l  = (case l of
--                     (x:xs)              -> (x:xs)
--                     _                   -> error "extraction failed" )
--             let (h:hs)          = extract_fail hl 
--             let (f:fs)          = extract_fail fl 
--             case (f, height == 0) of 
--                 (False, _)          -> State.put (hs,fs,ms)   >> return (toByteString h)
--                 (True,True)         -> State.put (hs,fs,h:ms) >> return (toByteString h)
--                 (True,False)        -> do 
--                     State.put (h:hs,fs,ms) 
--                     left <- accum ctw (height - 1) (2 * pos)
--                     if ctw(height-1) > 2*pos+1
--                         then do 
--                             right <- accum ctw (height - 1) (2 * pos) 
--                             return $ hash256 $ BS.concat [left,right]
--                         else 
--                             return $ hash256 $ BS.concat [left, left]
