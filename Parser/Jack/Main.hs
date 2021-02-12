module Main where


import Prelude hiding (EQ,GT,LTi,lex) 
import Parser
import Lexer 
import VMTranslate

import System.Directory  (getDirectoryContents, doesDirectoryExist, getCurrentDirectory) 
import System.Environment (getArgs) 
import System.FilePath.Posix 
import System.IO

import Control.Monad.State

isVMFile f = ".vm" == snd (splitExtension f) 

translate_all :: String -> [Term] -> State Int String 
translate_all f []       =  return ""  
translate_all f (x:xs)   =  state (\l -> 
                                let tr      = translate f x         in 
                                let tr_all  = translate_all f xs    in 
                                let l'      = execState tr l        in 
                                let str     = evalState tr l        in 
                                let l''     = execState tr_all l'   in 
                                let str'    = evalState tr_all l'   in 
                                (str ++ str' , l'') ) 

compile :: String -> String -> IO (State Int String) 
compile d f     =   (translate_all (remove_extension f) . parse . lex) <$> (hGetContents =<< openFile (d ++ "/" ++ f) ReadMode )
                where remove_extension = fst . splitExtension . snd . splitFileName 

compile_all :: String -> [String] -> IO ( State Int String )
compile_all d []        = return $ return "" 
compile_all d (f:fs)    = do    
                                tr      <- compile d f 
                                tr_all  <- compile_all d fs 
                                return $ state (\l -> 
                                    let l'      = execState tr l        in 
                                    let str     = evalState tr l        in 
                                    let l''     = execState tr_all l'   in 
                                    let str'    = evalState tr_all l'   in 
                                    (str ++ str' , l'') )




file o f        =   do 
    handle      <- openFile o WriteMode 
    mkcode      <- compile "." f
    let code    = evalState mkcode 1 
    hPutStr handle code  
    hClose handle 
    return () 

dir o d   = do
    files <- getDirectoryContents d 
    let vms = filter isVMFile files
    let vms' = sort vms 
    handle <- openFile o WriteMode 
    hPutStr handle VMTranslate.init 
    mkcode <- compile_all d  vms' 
    let code   = evalState mkcode 1 
    hPutStr handle code 
    hClose handle 
    return ()  

arg []              = (Nothing,[]) 
arg (x:xs)          =   case x of 
    "-o"                ->  let o:rest = xs in (Just o, rest) 
    _                   ->  let (out,ins) = arg xs in (out,x:ins)  

sort []             =   []
sort [x]            =   [x]
sort(x:xs)          =   let sorted = sort xs in 
                        if head xs == "Sys.vm"
                            then "Sys.vm" : x : tail xs 
                            else x:xs 

get_outfile out = case out of 
    Just f      -> f 
    Nothing     -> "out.asm" 

main    = do 
    args <- getArgs
    let (out,ins) = arg args 
    let (i:rest) = ins 
    let outfile  = i ++ "/" ++ (dropExtension . snd . splitFileName) i ++ ".asm"
    exists <- doesDirectoryExist i 
    if exists 
        then dir outfile i 
        else file outfile i   





