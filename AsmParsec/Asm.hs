{-# LANGUAGE BinaryLiterals #-} 
{-# LANGUAGE OverloadedStrings #-} 


module Main where 


import Prelude hiding (abs,lookup, head,tail ) 
import Data.Char
import Control.Monad
import Control.Applicative hiding (some,many) 
import Core 

-- import Data.Attoparsec.Text (parse) 



import qualified Data.HashTable.IO as H 
import Data.Bits 
-- import qualified Data.Text as T 
-- import qualified Data.Text.IO as TIO 
import Numeric (showIntAtBase) 
-- import Text.Printf 
import System.IO
import System.Directory (renameFile, getTemporaryDirectory) 
import System.Environment (getArgs) 




{------------------ Syntax --------------------} 

data Address    = VAR String 
                | ADDR Int   deriving ( Show, Eq )  

data Command    = A_ Address 
                | C_ Dest Comp Jump 
                | L_ String  deriving ( Show, Eq )

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



{-------------- HashTable --------------------} 

type Hashtbl k v = H.BasicHashTable k v 

add_tbl :: Hashtbl String Int -> String -> Int -> IO (Hashtbl String Int) 
add_tbl tbl k v = do 
    H.insert tbl k v
    return tbl 

init_tbl  :: IO (Hashtbl String Int) 
init_tbl  = do  
    tbl <- H.new 
    return tbl 

lookup tbl k = do 
    tbl <- tbl 
    v   <- H.lookup tbl k
    return v 


{----------------- Lexer ---------------------} 

syms        =   "+=-)(*&^%$#@!}{][/><.,\"\';:" 
heads       =   ['a'..'z'] ++ ['A'..'Z'] ++ ['_', '$', ':', '.']  
tails       =   ['0'..'9'] ++ heads 
head        =   oneOf heads         :: Lexer 
tail        =   list $ oneOf tails  

var         =   cons head <*> tail  

sp          =   list $ oneOf [' ','\t','\n','\r'] 

at          =   reserved "@" 
eq          =   reserved "=" 

comment     =   list $ oneOf $ tails ++ syms ++ " \t\r" 

{---------------- Parser ---------------------} 

-- file        ::  Parser [Command] 
-- file        =   ( do{ l <- line_; ls <- file; return $ l:ls })
-- file        =   ( infix_chainr1 line (do { newline; return $ (++) } ) )


line        =   ( do{ reserved "//"; comment ; return [] } ) 
            <|> ( do{ c <- command; return [c] } ) 
            <|> ( return [] ) 
            -- <|> ( return [] << eof ) 


-- line_       ::  Parser Command 
line_       =   ( reserved "//" >> comment >> empty ) 
            <|> ( command ) 
            <|> ( empty   ) 


-- command     :: Parser Command 
command     =   ( do{ at; sp; a <- symbol;  sp;       return  $ A_ a        } ) 
            <|> ( do{ l <- parens var;      sp;       return  $ L_ l        } )  
            <|> ( do{ d<-dest;sp;c<-comp;   sp;     return $ C_ d c NULL } )
            <|> ( do{ c<-comp;sp;j<-jump;   sp;     return $ C_ NONE c j } ) 
            <|> ( do{ d<-dest;sp;c<-comp;sp;j<-jump;sp; return $ C_ d c j } ) 

dest        ::  Parser Dest
dest        =   ( do{ reserved "A"  ; sp; eq; return A__  } )
            <|> ( do{ reserved "M"  ; sp; eq; return M__  } )
            <|> ( do{ reserved "D"  ; sp; eq; return D__  } )
            <|> ( do{ reserved "MD" ; sp; eq; return MD__ } )
            <|> ( do{ reserved "AM" ; sp; eq; return AM__ } )
            <|> ( do{ reserved "AD" ; sp; eq; return AD__ } )
            <|> ( do{ reserved "AMD"; sp; eq; return AMD__} )
            <|> ( do{ reserved ""   ; sp; eq; return NONE } ) 

comp        =   ( comprest =<< reg01 )  
            <|> ( do{ reserved"-"; r  <- reg01; return $ UMINUS r     } )
            <|> ( do{ reserved"!"; r  <- reg;   return $ NOT r        } )

comprest r  =   ( do{ reserved"+"; r' <- reg01; return $ PLUS  r r'   } )
            <|> ( do{ reserved"-"; r' <- reg01; return $ MINUS r r'   } )
            <|> ( do{ reserved"*"; r' <- reg01; return $ MUL   r r'   } )
            <|> ( do{ reserved"&"; r' <- reg01; return $ AND   r r'   } )
            <|> ( do{ reserved"|"; r' <- reg01; return $ OR    r r'   } )
            <|> ( return $ REG r ) 

jump        =   ( reserved";" >> sp >> jjj )
            <|> ( return NULL )

jjj         =   ( return JGT << reserved "JGT" ) 
            <|> ( return JEQ << reserved "JEQ" )
            <|> ( return JGE << reserved "JGE" )
            <|> ( return JLT << reserved "JLT" )
            <|> ( return JNE << reserved "JNE" )
            <|> ( return JLE << reserved "JLE" )
            <|> ( return JMP << reserved "JMP" )

symbol      ::  Parser Address 
symbol      =   ( return . ADDR     =<< uinteger )
            <|> ( return . VAR      =<< var      )

reg         ::  Parser Reg 
reg         =   ( return A << reserved "A" )
            <|> ( return M << reserved "M" )
            <|> ( return D << reserved "D" )

reg01       =   ( return ZERO << reserved "0" )
            <|> ( return ONE  << reserved "1" )
            <|> reg 

{------------------ Eval  --------------------} 

asm_dest d = case d of 
    NONE                -> 0b000
    M__                 -> 0b001
    D__                 -> 0b010
    MD__                -> 0b011
    A__                 -> 0b100
    AM__                -> 0b101
    AD__                -> 0b110
    AMD__               -> 0b111

asm_comp c  = case c of 
    REG     ZERO        -> 0b0101010
    REG     ONE         -> 0b0111111
    UMINUS  ONE         -> 0b0111010
    REG     D           -> 0b0001100
    REG     A           -> 0b0110000
    NOT     D           -> 0b0001101
    NOT     A           -> 0b0110001
    UMINUS  D           -> 0b0001111
    UMINUS  A           -> 0b0110011
    PLUS  D ONE         -> 0b0011111
    PLUS  A ONE         -> 0b0110111
    MINUS D ONE         -> 0b0010011
    MINUS A ONE         -> 0b0000111
    PLUS  D A           -> 0b0000010
    MINUS D A           -> 0b0010011
    MINUS A D           -> 0b0000111
    AND   D A           -> 0b0000000
    OR    D A           -> 0b0010101
    REG     M           -> 0b1110000
    NOT     M           -> 0b1110001
    UMINUS  M           -> 0b1110011
    PLUS  M ONE         -> 0b1110111
    MINUS M ONE         -> 0b1000111
    PLUS  D M           -> 0b1000010
    MINUS D M           -> 0b1010011
    MINUS M D           -> 0b1000111
    AND   D M           -> 0b1000000
    OR    D M           -> 0b1010101
    _                   -> error "Syntax Error: cannot asm COMP" 

asm_jump j = case j of 
    NULL                -> 0b000
    JGT                 -> 0b001
    JEQ                 -> 0b010
    JGE                 -> 0b011
    JLT                 -> 0b100
    JNE                 -> 0b101
    JLE                 -> 0b110
    JMP                 -> 0b111

shiftL' :: Int -> Int -> Int 
shiftL' = shift 

-- asm_cmd :: Command -> T.Text
asm_cmd cmd = case cmd of 
    A_ (ADDR a)         -> str_of_bin a 
    -- A_ (VAR k)          -> do { Just v <- lookup tbl k ; return $ T.pack $ str_of_bin v ++ "\n" }   
    C_ d c j            -> ( str_of_bin 
            $ asm_jump j + shiftL' ((+)(asm_comp c)(shiftL((+)(shiftL 0b111 3)(asm_dest d))7))3) 
    L_ _                -> ""


-- str_of_bin b = printf "%016s" $ showIntAtBase 2 intToDigit b ""  :: String 
str_of_bin b = fill0 $ showIntAtBase 2 intToDigit b "" 

-- fill0 = take 16 . ( \s -> repeat '0' ++ s ) 
fill0 xs = replicate (16 - length xs) '0' ++ xs   


{----------------- Main  ----------------------} 

addr    = 16 
tbl     = init_tbl :: IO (Hashtbl String Int) 

-- run     :: String   -> [Command]
-- run     =  runParser $ file 

-- run'    :: String -> [Command]
-- run'    =  runParser $ line 

run_ line_ =  runParser $ line_ 

map_run lines = foldl (\xs x -> 
        let p = parse line_ ( x) in 
        --let p = parse line_ ( T.unpack x) in 
        if [] == p then xs else 
            let [(p',_)] = p in 
            p' : xs ) [] lines 



main    = do 
    [filename] <- getArgs 
    let outFile = "a.hack"
    -- handle <- openFile WriteMode outFile 
    -- (tempName,tempHandle) <- openTempFile "." "temp" 
    -- tmpDir      <- getTemporaryDirectory 
    -- let tmpFile = tmpDir ++ "/" ++ filename 
    lines        <- (\str -> split '\n' str =<< readFile filename  
    -- let cmds    = run file
    -- let cmds    = map (run_ . T.unpack ) lines 
    let cmds = map_run lines 
    putStrLn "cmds are created" 
    writeFile outFile $ concat $ map asm_cmd cmds  
    -- hoge handle cmds 

{--
hoge h [] = hClose h
hoge h (c:cs) = do { TIO.hPutStr h =<< map asm_cmd c; hoge h cs } 
--} 


    -- let _ = map ( \x -> hPutStr tempHandle =<< asm_cmd tbl addrptr x) t
    -- hClose tempHandle 
    -- return ()

    -- let io_strings = fmap (asm_cmd tbl addrptr) t
    -- let io_string = hoge io_strings 
    -- writeFile outFile =<< io_string 
    -- mapM ( \x -> writeFile outFile =<< asm_cmd tbl addrptr x ) t 

{--
hoge :: [IO String] -> IO String 
hoge [] = return ""
hoge (x:xs) = do 
    s  <- x 
    ss <- hoge xs 
    return $ s ++ ss
--} 

{--
main    :: IO ()

main    = forever $ do
    putStr "λ >> "; 
    a <- getLine; 
    let t = run' a ; 
    let tbl = init_tbl :: IO (Hashtbl String Int) 
    let addrptr = 16
    case t of 
        []      -> print "" 
        x:[]    -> do { print x ; print =<< asm_cmd tbl addrptr x } 
--}

