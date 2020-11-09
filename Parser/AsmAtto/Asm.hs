{-# LANGUAGE BinaryLiterals #-} 
{-# LANGUAGE OverloadedStrings #-} 

module Main where 

import Core 

import Prelude                          hiding (abs,lookup, head,tail ) 
import Data.Char
import Control.Applicative              hiding (some,many) 
import Data.Attoparsec.ByteString.Char8 hiding (D, count) 

import Data.HashMap                     hiding (map) 
import qualified Data.ByteString.Char8  as B  
import Numeric      (showIntAtBase) 
import System.IO
import System.Environment (getArgs) 

import Control.DeepSeq
import Data.Time


{------------------ Syntax --------------------} 

data Address    = VAR String 
                | ADDR Int   deriving ( Show, Eq )  

data Command    = A_ Address 
                | C_ Dest Comp Jump 
                | L_ String  deriving ( Show, Eq )

instance NFData Command where 
    rnf _ = () 

data Dest       = NONE | M__ | D__ | MD__
                | A__ | AM__ | AD__ | AMD__ deriving (Show,Eq) 

data Comp       = PLUS  Reg Reg 
                | MINUS Reg Reg 
                | MUL   Reg Reg 
                | AND   Reg Reg 
                | OR    Reg Reg 
                | NOT       Reg 
                | UMINUS    Reg 
                | REG       Reg deriving (Show,Eq) 

data Jump       = NULL
                | JGT
                | JEQ
                | JGE
                | JLT
                | JNE
                | JLE
                | JMP deriving (Eq,Show)

data Reg        = A | D | M | ONE | ZERO  deriving (Eq,Show) 


{----------------- Lexer ---------------------} 

syms        =   "+-)(*&^%#!}{][/><,\"\';" 
heads       =   ['a'..'z'] ++ ['A'..'Z'] ++ ['_', '$', ':', '.']  
tails       =   ['0'..'9'] ++ heads 
head        =   oneOf heads         :: Lexer 
tail        =   list $ oneOf tails  


var         =   do { h <- head; tl <- tail; sp; return $ h:tl }    

sp          =   spaces 

at          =   "@" 
eq          =   "="

comment     =   list $ oneOf $ syms ++ tails ++ " \t\r"  

{---------------- Parser ---------------------} 


line        =   ( command >>= return . Just   ) 
            <|> ( comment >> return Nothing )

command     :: Parser Command 
command     =   ( "@" >> symbol  >>= return . A_     ) 
            <|> ( parens var     >>= return . L_     ) 
            <|> ( c_command )

c_command   =   ( "A" >> a_rest ) 
            <|> ( "M" >> m_rest ) 
            <|> ( "D" >> d_rest ) 
            <|> ( "0" >> zero_rest ) 
            <|> ( "1" >> one_rest ) 
            
a_rest      =   ( "M" >> am_rest )
            <|> ( "D" >> ad_rest )
            <|> ( eq >> eq_rest A__ )
            <|> ( none_rest A       )

m_rest      =   ( "D" >> md_rest ) 
            <|> ( eq >> eq_rest M__ )
            <|> ( none_rest     M   )

d_rest      =   ( eq >> eq_rest D__ ) 
            <|> ( none_rest     D   ) 

zero_rest   =   ( none_rest ZERO )
one_rest    =   ( none_rest ONE  )

md_rest     =   ( eq >> eq_rest MD__ ) 
ad_rest     =   ( eq >> eq_rest AD__ ) 
am_rest     =   ( "D=" >> eq_rest AMD__) 
            <|> ( eq >> eq_rest AM__ )

eq_rest d   = do { c<-comp;         j<-jump; return $ C_ d    c j }

none_rest r = do { c<-comprest r;   j<-jump; return $ C_ NONE c j } 

comp        =   ( "-" >> reg01 >>= return . UMINUS  )
            <|> ( "!" >> reg   >>= return . NOT     )
            <|> ( reg01 >>= comprest )

comprest r  =   ( "+" >> reg01 >>= return . PLUS  r )
            <|> ( "-" >> reg01 >>= return . MINUS r )
            <|> ( "*" >> reg01 >>= return . MUL   r )
            <|> ( "&" >> reg01 >>= return . AND   r )
            <|> ( "|" >> reg01 >>= return . OR    r )
            <|> (            (return $ REG   r)) 

jump        =   ( ";" >> jjj )
            <|> ( sp >>                     return NULL )

jjj         =   ( reserved "JGT" >>         return JGT ) 
            <|> ( reserved "JEQ" >>         return JEQ )
            <|> ( reserved "JGE" >>         return JGE )
            <|> ( reserved "JLT" >>         return JLT )
            <|> ( reserved "JNE" >>         return JNE )
            <|> ( reserved "JLE" >>         return JLE )
            <|> ( reserved "JMP" >>         return JMP )

symbol      ::  Parser Address 
symbol      =   ( uinteger >>= return . ADDR )
            <|> ( var      >>= return . VAR  )

reg         ::  Parser Reg 
reg         =   ( reserved "A" >> return A )
            <|> ( reserved "M" >> return M )
            <|> ( reserved "D" >> return D )

reg01       =   ( reserved "0" >> return ZERO )
            <|> ( reserved "1" >> return ONE  )
            <|> reg 

{------------------ Eval  --------------------} 

asm_dest d = case d of 
    NONE                -> (:)'0' . (:)'0' . (:)'0'
    M__                 -> (:)'0' . (:)'0' . (:)'1'
    D__                 -> (:)'0' . (:)'1' . (:)'0'
    MD__                -> (:)'0' . (:)'1' . (:)'1'
    A__                 -> (:)'1' . (:)'0' . (:)'0'
    AM__                -> (:)'1' . (:)'0' . (:)'1'
    AD__                -> (:)'1' . (:)'1' . (:)'0'
    AMD__               -> (:)'1' . (:)'1' . (:)'1'

asm_comp c  = case c of 
    REG     ZERO        -> (:)'0' . (:)'1' . (:)'0' . (:)'1' . (:)'0' . (:)'1' . (:)'0' 
    REG     ONE         -> (:)'0' . (:)'1' . (:)'1' . (:)'1' . (:)'1' . (:)'1' . (:)'1' 
    UMINUS  ONE         -> (:)'0' . (:)'1' . (:)'1' . (:)'1' . (:)'0' . (:)'1' . (:)'0' 
    REG     D           -> (:)'0' . (:)'0' . (:)'0' . (:)'1' . (:)'1' . (:)'0' . (:)'0' 
    REG     A           -> (:)'0' . (:)'1' . (:)'1' . (:)'0' . (:)'0' . (:)'0' . (:)'0' 
    NOT     D           -> (:)'0' . (:)'0' . (:)'0' . (:)'1' . (:)'1' . (:)'0' . (:)'1' 
    NOT     A           -> (:)'0' . (:)'1' . (:)'1' . (:)'0' . (:)'0' . (:)'0' . (:)'1' 
    UMINUS  D           -> (:)'0' . (:)'0' . (:)'0' . (:)'1' . (:)'1' . (:)'1' . (:)'1' 
    UMINUS  A           -> (:)'0' . (:)'1' . (:)'1' . (:)'0' . (:)'0' . (:)'1' . (:)'1' 
    PLUS  D ONE         -> (:)'0' . (:)'0' . (:)'1' . (:)'1' . (:)'1' . (:)'1' . (:)'1' 
    PLUS  A ONE         -> (:)'0' . (:)'1' . (:)'1' . (:)'0' . (:)'1' . (:)'1' . (:)'1' 
    MINUS D ONE         -> (:)'0' . (:)'0' . (:)'0' . (:)'1' . (:)'1' . (:)'1' . (:)'0' 
    MINUS A ONE         -> (:)'0' . (:)'1' . (:)'1' . (:)'0' . (:)'0' . (:)'1' . (:)'0' 
    PLUS  D A           -> (:)'0' . (:)'0' . (:)'0' . (:)'0' . (:)'0' . (:)'1' . (:)'0' 
    MINUS D A           -> (:)'0' . (:)'0' . (:)'1' . (:)'0' . (:)'0' . (:)'1' . (:)'1' 
    MINUS A D           -> (:)'0' . (:)'0' . (:)'0' . (:)'0' . (:)'1' . (:)'1' . (:)'1' 
    AND   D A           -> (:)'0' . (:)'0' . (:)'0' . (:)'0' . (:)'0' . (:)'0' . (:)'0' 
    OR    D A           -> (:)'0' . (:)'0' . (:)'1' . (:)'0' . (:)'1' . (:)'0' . (:)'1' 
    REG     M           -> (:)'1' . (:)'1' . (:)'1' . (:)'0' . (:)'0' . (:)'0' . (:)'0' 
    NOT     M           -> (:)'1' . (:)'1' . (:)'1' . (:)'0' . (:)'0' . (:)'0' . (:)'1' 
    UMINUS  M           -> (:)'1' . (:)'1' . (:)'1' . (:)'0' . (:)'0' . (:)'1' . (:)'1' 
    PLUS  M ONE         -> (:)'1' . (:)'1' . (:)'1' . (:)'0' . (:)'1' . (:)'1' . (:)'1' 
    MINUS M ONE         -> (:)'1' . (:)'1' . (:)'1' . (:)'0' . (:)'0' . (:)'1' . (:)'0' 
    PLUS  D M           -> (:)'1' . (:)'0' . (:)'0' . (:)'0' . (:)'0' . (:)'1' . (:)'0' 
    MINUS D M           -> (:)'1' . (:)'0' . (:)'1' . (:)'0' . (:)'0' . (:)'1' . (:)'1' 
    MINUS M D           -> (:)'1' . (:)'0' . (:)'0' . (:)'0' . (:)'1' . (:)'1' . (:)'1' 
    AND   D M           -> (:)'1' . (:)'0' . (:)'0' . (:)'0' . (:)'0' . (:)'0' . (:)'0' 
    OR    D M           -> (:)'1' . (:)'0' . (:)'1' . (:)'0' . (:)'1' . (:)'0' . (:)'1' 
    _                   -> error "Syntax Error: cannot asm COMP" 

asm_jump j = case j of 
    NULL                -> (:)'0' . (:)'0' . (:)'0'
    JGT                 -> (:)'0' . (:)'0' . (:)'1'
    JEQ                 -> (:)'0' . (:)'1' . (:)'0'
    JGE                 -> (:)'0' . (:)'1' . (:)'1'
    JLT                 -> (:)'1' . (:)'0' . (:)'0'
    JNE                 -> (:)'1' . (:)'0' . (:)'1'
    JLE                 -> (:)'1' . (:)'1' . (:)'0'
    JMP                 -> (:)'1' . (:)'1' . (:)'1'



map_asm_cmd counter tbl []      = []
map_asm_cmd counter tbl (x:xs)  = case x of
    A_ (ADDR a)         -> asm_Acmd a : map_asm_cmd counter tbl xs 
    A_ (VAR k)          -> ( case lookup k tbl of 
        Just v              -> asm_Acmd v       : map_asm_cmd  counter      tbl                  xs
        Nothing             -> asm_Acmd counter : map_asm_cmd (counter+1) (insert k counter tbl) xs )
    C_ d c j            -> asm_Ccmd d c j : map_asm_cmd counter tbl xs 


asm_Acmd        = B.pack . fill0 . toStr 
asm_Ccmd d c j  = B.pack $ (:)'1' . (:)'1' . (:)'1' . asm_comp c . asm_dest d . asm_jump j $ []

toStr b     = showIntAtBase 2 intToDigit b "" 
fill0 str   = replicate (16 - length ys) '0' ++ ys where ys = Prelude.take 16 str   


{-------------- HashTable --------------------} 

type Tbl = Map String Int 

symboltbl :: Tbl 
symboltbl = fromList $ [("SP",0),("LCL",1),("ARG",2),("THIS",3),("THAT",4)] 
        ++ ( map (\n -> ( "R" ++ show n , n )) [0..15] ) 
        ++ [("SCREEN",0x4000),("KBD",0x2000)]

{----------------- Run Parse -------------------} 

maprun line tbl []       = ([], tbl) 
maprun line tbl (x: xs)  =  
        case run x of 
        Just cmd    -> ( case cmd of 
            Just (L_ label)        -> ( maprun line (insert label line tbl) xs ) 
            Just  cmd              -> ( cmd:cmds, tbl') where (cmds, tbl') = maprun (line+1) tbl xs 
            Nothing                -> ( maprun line tbl xs ) ) 
        Nothing     -> maprun line tbl xs

map_run l = maprun 0 symboltbl l 

run = runParser line  


{----------------- Main  ----------------------} 

main    = do 
    [filename] <- time "args " $ getArgs 
    let name:ext:_ = B.split '.' $ B.pack filename 
    let outFile = B.unpack name ++ ".hack" 
    handle      <- openFile outFile WriteMode
    lines       <- time "input" $ B.split '\n' <$> B.readFile filename  
    (cmds,tbl') <- time "parse" $ return $ map_run lines 
    codes       <- time "assem" $ return $ map_asm_cmd 16 tbl' cmds
    time "write" $ write_File codes handle 
    ---- code       <- time "unline"$ return $ B.unlines codes 
    ---- time "write" $ B.writeFile outFile $ code     
    ------ time "assem" $ mapM ( B.hPutStrLn handle . asm_cmd ) cmds
    ------ hClose handle

write_File []   handle         = do 
    hClose handle  
write_File (code:rest)  handle = do 
    B.hPutStrLn handle code 
    write_File rest handle 

{--
main = forever $ do 
    putStr " >>> "
    a <- getLine 
    let cmd = run $ B.pack a 
    case cmd of 
        Just c      -> print $ asm_cmd c
        Nothing     -> return ()
--}


{----------------- Time ---------------------} 

time str io_monad = do 
    t0          <- getCurrentTime
    result      <- io_monad
    t1          <- result `deepseq` getCurrentTime 
    putStrLn $ str ++ ": " ++ show (diffUTCTime t1 t0) 
    return result 

