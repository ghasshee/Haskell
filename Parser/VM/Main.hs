module Main where


import Prelude hiding (EQ,GT,LTi,lex) 
import Parser
import Lexer 
import VMTranslate

import System.Directory  (getDirectoryContents, doesDirectoryExist, getCurrentDirectory) 
import System.Environment (getArgs) 
import System.FilePath.Posix 
import System.IO

isVMFile f = ".vm" == snd (splitExtension f) 

compile d f =  ( mapM putStr . map (translate f') . parse . lex ) =<< hGetContents =<< openFile (d ++ "/" ++ f) ReadMode
                where f' = fst (splitExtension (snd (splitFileName f)) )

{--
compile f = do 
    handle <- openFile f ReadMode
    conte hGetContents =<< openFile f ReadMode
--} 
--

dir d   = do
    files <- getDirectoryContents d 
    let vms = filter isVMFile files
    mapM (compile d) vms 
    return () 



main    = do 
    args <- getArgs
    let arg:rest = args 
    exists <- doesDirectoryExist arg 
    if exists 
        then dir arg 
        else do { compile "." arg ; return () } 
    -- s <- getContents
    -- mapM putStr $ map translate $ parse $ lex s 





