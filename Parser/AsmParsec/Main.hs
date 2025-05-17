module Main where 


import Asm 
import System.Directory (renameFile, getTemporaryDirectory) 
import System.Environment (getArgs) 


main = do 
    [file] <- getArgs 
    tmpDir <- getTemporaryDirectory 
    let tmpFile = tmpDir ++ "/" ++ file 
    readFile file >>= writeFile tmpFile . map id  
    renameFile tmpFile file 

