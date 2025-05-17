module Main where 

import Tool
import Prelude hiding (lex) 
import Data.Char (chr,ord,toLower) 
import Data.Maybe (fromJust) 
import System.Directory  (getDirectoryContents, doesDirectoryExist, getCurrentDirectory) 
import System.Environment (getArgs) 
import System.FilePath.Posix 
import System.IO  

import Control.Monad.State
import Control.Monad.Extra (ifM) 

-----------------------------------------------------------
---             TOKEN TYPES                             ---
-----------------------------------------------------------
    

data Keyword    = CLASS | METHOD | FUNCTION | CONSTRUCTOR 
                | INT | BOOLEAN | CHAR | VOID 
                | VAR | STATIC | FIELD | THIS 
                | LET | DO | IF | ELSE | WHILE | RETURN 
                | TRUE | FALSE | NULL       deriving (Show) 

type Symbol     = Char

type Identifier = String 

data Token      = KEYWORD Keyword 
                | SYMBOL Symbol
                | IDENTIFIER Identifier 
                | INT_CONST Int 
                | STRING_CONST String       deriving (Show) 

data TokenType  = Keyword | Symbol | Identifier | Int | String      
                                            deriving (Show)  


-----------------------------------------------------------
---             LEXER MODULES                           ---
-----------------------------------------------------------
    
hasMoreTokens   ::  Handle -> IO Bool 
hasMoreTokens h =   return . not =<< hIsEOF h   
eof             =   hIsEOF

advance         ::  Handle -> IO Char 
advance         =   hGetChar 

keyword         ::  String -> Maybe Keyword
keyword str     =   case str of 
    "class"         -> return CLASS 
    "method"        -> return METHOD
    "function"      -> return FUNCTION
    "constructor"   -> return CONSTRUCTOR
    "int"           -> return INT
    "char"          -> return CHAR
    "boolean"       -> return BOOLEAN
    "void"          -> return VOID
    "this"          -> return THIS
    "let"           -> return LET
    "do"            -> return DO 
    "if"            -> return IF 
    "else"          -> return ELSE
    "while"         -> return WHILE
    "return"        -> return RETURN
    "true"          -> return TRUE
    "false"         -> return FALSE
    _               -> Nothing  

isSymbol    [c] =   elem c "{}()[].,;+-*/&|<>=~"  
isSymbol     _  =   False
intVal       s  =   if isDigits s then return(read s) else Nothing                    
isDigits        =   foldr ((.)(&&)isDigit) True
isDigit      d  =   elem d "0123456789"    
isLetter     c  =   (elem c . map chr)([65..90]++[97..122])
isIdentifier s  =   if isDigit (head s) then False  
                    else foldr ((.)(&&)((|||)[isDigit,isLetter,(=='_')]))True s  
isString     s  =   head s=='"' && last s=='"' && 
                    (foldr ((...)(&&)(/=)'"') True $ tail $ init s)
isWhiteSpace c      = c==' ' || c=='\t' || c=='\n' || c=='\r' 

symbol    [c]   =   if isSymbol [c]     then Just c     else Nothing     
symbol     _    =                                            Nothing
string     s    =   if isString s       then Just s     else Nothing 
identifier s    =   if isIdentifier s   then Just s     else Nothing 

-----------------------------------------------------------
---          LEXER : STRINGS -> TOKENS                  ---
-----------------------------------------------------------
    
xml_of_token token = case token of 
    KEYWORD a       -> "<keyword> "         ++ map toLower (show a) ++" </keyword>" 
    SYMBOL  a       -> "<symbol> "          ++ [a]    ++" </symbol>" 
    INT_CONST a     -> "<integerConstant> " ++ show a ++" </integerConstant>" 
    STRING_CONST a  -> "<stringCOnstant> "  ++ a      ++" </stringConstant>"
    IDENTIFIER a    -> "<identifier> "      ++ a      ++" </identifier>"

tokenize            :: String -> Token 
tokenize str        =  case keyword str  of 
    Just a  -> KEYWORD a 
    Nothing -> case symbol str of 
        Just a  -> SYMBOL a 
        Nothing -> case intVal str of 
            Just a  -> INT_CONST a 
            Nothing -> case identifier str of 
                Just a  -> IDENTIFIER a 
                Nothing -> case string str of 
                    Just a  -> STRING_CONST a  
                    Nothing -> error $ "Tokenization Failed: " ++ (show $ map ord str)  

tokenType           :: Token -> TokenType
tokenType token     = case token of 
    KEYWORD _           -> Keyword
    SYMBOL _            -> Symbol 
    IDENTIFIER _        -> Identifier
    INT_CONST _         -> Int 
    STRING_CONST _      -> String 


-----------------------------------------------------------
---          LEXER : STREAM -> STRINGS                  ---
-----------------------------------------------------------

getTokens h lookahead = do 
    ifM (eof h) (return []) $ do 
    (token,next)    <- case lookahead of 
            Nothing     -> getToken h [] 
            Just a      -> getToken h [a]
    let n           =  case next of 
            Nothing     -> []
            Just a|isWhiteSpace a -> []
            Just a      -> [[a]]
    ifM (eof h) (return $ [token] ++ n ) $ do 
    xs      <- getTokens h next 
    return $ token : xs

getToken h str      = do
    if str=="/" 
        then do next <- advance h 
                if next=='/'
                    then do next'   <- getComment h 
                            case next' of 
                                Just a  -> getToken h [a]
                                Nothing -> getToken h []
                    else if next=='*' 
                            then do next'  <- getComment2 h
                                    case next' of 
                                        Just a  -> getToken h [a]
                                        Nothing -> getToken h []
                            else if isWhiteSpace next 
                                    then do next' <- skipWhiteSpaces h
                                            return ("/",next')
                                    else return ("/", Just next) 
        else do  
    if isSymbol str 
        then do next <- skipWhiteSpaces h 
                return (str,next) 
        else do 
    if str==['"']
        then getString h str
        else do 
    if str==""
        then do next <- advance h 
                getToken h [next]
        else do 
    next        <- advance h 
    ifM (eof h) (return (str,Nothing)) $ do 
    if isSymbol [next] 
        then return (str,Just next)  
        else do 
    if isWhiteSpace next 
        then do next <- skipWhiteSpaces h 
                return (str,next) 
        else getToken h (str++[next])

getString h str     = do
    char        <- advance h 
    ifM (eof h) (error "LEXER ERROR: STRING NOT CLOESED") $ do 
    let str'    = str ++ [char] 
    if char=='"'
        then do next <- skipWhiteSpaces h 
                return (str',next)   
        else getString h str'   

getComment h        = do 
    next        <- advance h 
    if next=='\r' || next=='\n' 
        then do next' <- skipWhiteSpaces h 
                return next' 
        else getComment h 

getComment2 h       = do 
    next            <- advance h 
    let endComment c = if c=='/' 
                            then do next' <- skipWhiteSpaces h 
                                    return next' 
                            else if c=='*' 
                                    then do next' <- advance h 
                                            endComment next'  
                                    else getComment2 h 
    if next=='*' 
        then do next'   <- advance h 
                endComment next' 
        else getComment2 h 

skipWhiteSpaces h = do
    char <- advance h  
    ifM (eof h) ((.)return Just char) $ do 
    if isWhiteSpace char 
        then skipWhiteSpaces h 
        else return $ Just char



-----------------------------------------------------------
---          LEXER : INPUT  ->  OUTPUT                  ---
-----------------------------------------------------------

lex                 ::  Handle -> IO [String]  
lex handle          =   return . map(xml_of_token . tokenize) =<< getTokens handle Nothing   



-----------------------------------------------------------
---          FILE -> STREAM                             ---
-----------------------------------------------------------

transform_file d f  =   do  
    handle              <-  openFile (d++"/"++f) ReadMode  
    tokens              <-  lex handle 
    return $ state (\l -> (tokens , l))  

transform_files d []        = return $ return [""] 
transform_files d (f:fs)    = do    
    tr                  <-  transform_file d f 
    tr_all              <-  transform_files d fs 
    return $ state (\l -> 
        let l'          =   execState tr l        in 
        let str         =   evalState tr l        in 
        let l''         =   execState tr_all l'   in 
        let str'        =   evalState tr_all l'   in 
        (str ++ str' , l'') )

read_file           :: String -> IO [String] 
read_file  f        =   do 
    mkcode              <-  transform_file "." f
    let code            =   evalState mkcode 1 
    return code 

read_dir            :: String -> IO [String] 
read_dir  d         = do
    files               <-  getDirectoryContents d 
    let jacks           =   filter isJackFile files
    mkcode              <-  transform_files d jacks
    let code            =   evalState mkcode 1 
    return code 

-----------------------------------------------------------
---          LEXER : STREAM -> STRINGS                  ---
-----------------------------------------------------------


isVMFile            ::  String -> Bool 
isVMFile f          =   ".vm"     == snd (splitExtension f) 
isJackFile f        =   ".jack"   == snd (splitExtension f) 

remove_extension    ::  String -> String 
remove_extension    =   fst . splitExtension . snd . splitFileName 

arg []              = (Nothing,[]) 
arg (x:xs)          =   case x of 
    "-o"                ->  let o:rest = xs in (Just o, rest) 
    _                   ->  let (out,ins) = arg xs in (out,x:ins)  

main    = do 
    ---- GET ARGS 
    args <- getArgs
    let (out,ins) = arg args 
    ---- READ FILES
    let (i:rest) = ins 
    exists <- doesDirectoryExist i 
    ---- TRANSFORM
    code <- if exists 
                then read_dir i 
                else read_file i   
    ---- WRITE FILE 
    let outfile  = if exists 
        then i ++ "/" ++ (dropExtension . snd . splitFileName) i ++ ".vm"
        else (dropExtension . snd . splitFileName) i ++ ".vm" 
    outHandle  <- openFile outfile WriteMode 
    mapM (hPutStr outHandle) code
    hClose outHandle



