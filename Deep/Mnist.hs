module Mnist where 

import Control.Monad
import Numeric.LinearAlgebra 
import Network.HTTP.Simple (parseRequest, httpLBS, getResponseBody) 
import System.Directory (doesFileExist) 

import qualified Data.ByteString.Lazy as BL 
import qualified Codec.Compression.GZip as GZ (compress, decompress) 

import Data.Binary (encode, decode) 

type DataSet = (Matrix R, Matrix R)

baseURL     = "http://yann.lecun.com/exdb/mnist"

keyFiles    = [
            ("train_img",   "train-images-idx3-ubyte.gz"),
            ("train_label", "train-labels-idx1-ubyte.gz"),
            ("test_img",    "t10k-images-idx3-ubyte.gz"),
            ("test_label",  "t10k-labels-idx1-ubyte.gz")
            ]

assetsDir   = "assets"
pickleFile  = "mnist.dat"

imgSize     = 784

genPath     :: String -> String
genPath p   = assetsDir ++ "/" ++ p 

download    :: String -> IO ()
download f  = do 
    let savePath = genPath f 
    
    e   <- doesFileExist savePath 
    unless e $ do 
        putStrLn $ "Downloading " ++ f ++ " ... " 
        res <- httpLBS =<< parseRequest (baseURL ++ "/" ++ f) 
        BL.writeFile savePath (getResponseBody res) 
        putStrLn "Done" 


downloadMnist :: [(String, String)] -> IO ()
downloadMnist []        = return ()
downloadMnist (x:xs)    = do
    download $ snd x
    downloadMnist xs 


toDoubleList    :: BL.ByteString -> [Double] 
toDoubleList    = map (read . show . fromEnum) . BL.unpack 

loadLabel       :: String -> IO (Matrix R) 
loadLabel f     = do 
    contents <- fmap GZ.decompress (BL.readFile $ genPath f) 
    return . matrix 1 . toDoubleList $ BL.drop 8 contents

loadImg         :: String -> IO (Matrix R)
loadImg f       = do 
    contents <- fmap GZ.decompress (BL.readFile $ genPath f) 
    return . matrix imgSize . toDoubleList $ BL.drop 16 contents


toMatrix        :: IO [DataSet] 
toMatrix        = do 
    trainImg        <- loadImg      . snd $ keyFiles !! 0 
    trainLabel      <- loadLabel    . snd $ keyFiles !! 1 
    testImg         <- loadImg      . snd $ keyFiles !! 2
    testLabel       <- loadLabel    . snd $ keyFiles !! 3
    return [(trainImg,trainLabel), (testImg, testLabel)]




createPickle        :: String -> [DataSet] -> IO ()
createPickle p ds   = BL.writeFile p $ (GZ.compress . encode) ds 

loadPickle      :: String -> IO [DataSet] 
loadPickle p    = do 
    encodeDs <- BL.readFile p 
    return $ (decode . GZ.decompress) encodeDs


initMnist           :: IO ()
initMnist           =  do 
    downloadMnist keyFiles 
    putStrLn "Creating binary Matrix file ... "
    createPickle (genPath pickleFile) =<< toMatrix 
    putStrLn "Done" 

normalizeImg        :: Bool -> [DataSet] -> IO [DataSet] 
normalizeImg f ds@[train,test] 
    | f             = return [((/255) $ fst train, snd train), ((/255) $ fst test, snd test)]
    | otherwise     = return ds 


loadMnist           :: Bool -> IO [DataSet] 
loadMnist normalize = do 
    let loadPath    = genPath pickleFile 
    e <- doesFileExist loadPath
    unless e initMnist 
    loadPickle loadPath >>= normalizeImg normalize



from784to28x28 [] = [] 
from784to28x28 xs = [take 28 xs] : from784to28x28 (drop 28 xs)


showI [] = ""
showI (x:xs) = show x ++ "\n" ++ showI xs

instance {-# Overlaps #-} Show [[Double]] where 
    show = showI 


test = do 
    [ts,ds] <- loadMnist False
    mapM (\n -> print $ from784to28x28 $ toList $ (fst ds)! n) [0..9] 
