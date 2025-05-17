module Main where 

import Prelude hiding (lex) 
import Lexer 
import Parser
import System.IO 

main = do 
    done <- isEOF
    if done then return () else do 
        -- s <- getContents
        s <- getLine 
        print $ flip parse [] . lex $ s 
        main 

