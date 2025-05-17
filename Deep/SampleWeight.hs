module SampleWeight where 


import Control.Applicative ((<$>))
import Control.Monad
import Numeric.LinearAlgebra
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZ (compress, decompress)
import Data.Binary (encode,decode)
import Text.Parsec

type Weight = Matrix R
type Bias   = Vector R 
type SampleWeight = ([Weight], [Bias]) 

dir     =   "assets"
files   =   [ "sample-weight-w1"
            , "sampel-weight-w2"
            , "sampel-weight-w3"
            , "sampel-weight-b1"
            , "sampel-weight-b2"
            , "sampel-weight-b3" ] 

genPath p = dir ++ "/" ++ p 

createBinary :: String -> IO ()
createBinary p = do 
    let bp = genPath p 
    ws <- readFile $ bp ++ ".csv"
    case parseCSV ws of 
        Left e -> print e 
        Right w -> do 
            putStrLn $ "Creating binary Matrix file: " ++ bp 
            let wm = fromLists $ fmap (read :: String -> Double) <$> w 
            createPickle (bp ++ ".dat") wm
            putStrLn "Done" 
            
csvSyntax   = endBy line eol 
line        = sepBy cell $ char ','
cell        = many $ noneOf ",\n"
eol         = char '\n'

parseCSV = parse csvSyntax "* ParseError *"

createPickle p w = BL.writeFile p $ (GZ.compress. encode) w 

loadPickle p = do 
    esw <- BL.readFile $ genPath p ++ ".dat" 
    return $ decode $ GZ.decompress esw 

loadSW :: IO SampleWeight 
loadSW = do 
    sw <- forM files loadPickle 
    let (w,b) = splitAt 3 sw 
    return (w, fmap flatten b) 
