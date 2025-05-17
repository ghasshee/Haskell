module Main where 

import Syntax
import Lexer
import Parser
import Eval

import Prelude hiding (abs, lex, True, False)
import Data.Char
import Control.Monad
import Control.Applicative hiding (some,many) 
import System.IO (hFlush, stdout) 



-- type Context = [String] 


{----------------- Main  ----------------------} 

run         ::  String   -> Term
run         =   flip parse [] . lex 


main        ::  IO ()
main        =   forever $ do
    putStr "λ> "
    hFlush stdout
    a <- getLine; 
    let t = run a ; 
    putStrLn $ show  t  
    putStrLn " ---------- evaluation ----------- " 
    let e = eval [] t;  
    putStrLn $ show e 



