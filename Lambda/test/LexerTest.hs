module Main where 

import Lexer 
import System.IO 

main = do 
    done <- isEOF
    if done then return () else do 
        -- s <- getContents
        s <- getLine 
        print (alexScanTokens s) 
        main 

