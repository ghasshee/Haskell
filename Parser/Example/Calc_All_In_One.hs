module Main  where 

import Core
import Data.Char
import Control.Applicative
import Control.Monad

------- Syntax -------
data Expr   = Add Expr Expr
            | Mul Expr Expr
            | Sub Expr Expr
            | Lit Integer       deriving Show

------- Lexer -------
-- chainl              ::  Parser a -> Parser(a->a->a) -> a -> Parser a
-- chainl pa pop a     =   (pa `chainl1` pop) <|> return a
chainl1             ::  Parser a -> Parser(a->a->a) -> Parser a 
p `chainl1` op      =   do { a<-p; rest p op a }
rest pa pop a       =   do { op<-pop; b<-pa; rest pa pop (op a b) } <|> return a  
(#)                 =   chainl1 

elem_of s c         =   flip elem s c 
oneOf str           =   satisfy (elem_of str)                   :: Parser Char  
token   p           =   do { a <- p; spaces ; return a }        :: Parser a 
char  c             =   satisfy (c ==)                          :: Parser Char
digit               =   satisfy isDigit                         :: Parser Char 
nat                 =   read <$> natStr                         :: Parser Integer
natStr              =   some (satisfy isDigit)                  :: Parser String
str  ""             =   return []                               
str  (c:cs)         =   do { char c; str cs; return (c:cs)}     :: Parser String 
reserved s          =   token (str s)                           :: Parser String 
spaces              =   many $ oneOf " \n\r"                    :: Parser String 
number              ::  Parser Integer
number              =   do{ s<-str"-" <|> return[]; cs<-some digit; return $ read(s++cs) }   
num                 =   token number 
parens              ::  Parser a -> Parser a
parens m            =   do{ reserved"("; spaces; n<-m; spaces; reserved")"; return n }

------- Parser -------
int             =   num >>= return . Lit                        :: Parser Expr 
expr            =   term # addop                                :: Parser Expr
term            =   factor # mulop                              :: Parser Expr
factor          =   int <|> parens expr                         :: Parser Expr 
infixOp x f     =   reserved x >> return f                      :: Parser(Expr->Expr->Expr)
addop           =   (infixOp "+" Add) <|> (infixOp "-" Sub)     :: Parser(Expr->Expr->Expr)
mulop           =   infixOp "*" Mul                             :: Parser(Expr->Expr->Expr) 

run             =   runParser expr                              :: String -> Expr 

-------  Eval  ------- 
eval        :: Expr -> Integer
eval ex     = case ex of 
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b
    Sub a b -> eval a - eval b
    Lit n   -> n

-------  Main  -------
main    :: IO ()
main    = forever $ do
    putStr "Calculator> "
    a <- getLine
    print $ eval $ run a


{------- Backus-Naur Form --------
 -
 -  number   = [ - ] digit { digit }.
 -  digit    = 0 | 1 | ... | 8 | 9.
 -  expr     = term   { addop term   }.
 -  term     = factor { mulop factor }.
 -  factor   = ( expr ) | number.
 -  addop    = + | -
 -  mulop    = *
 -
 ---------------------------------}


