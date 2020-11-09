module Parser where 

import Core
import Data.Char
import Control.Applicative
import Control.Monad

oneOf               ::  [Char]   -> Parser Char 
oneOf s             =   satisfy (elem_of s) 
elem_of s c         =   flip elem s c 

-- chainl              ::  Parser a -> Parser (a -> a -> a) -> a -> Parser a
-- chainl pa pop a     =   (pa `chainl1` pop) <|> return a

chainl1             ::  Parser a -> Parser (a -> a -> a) -> Parser a 
p `chainl1` op      =   do { a <- p; rest p op a }
rest pa pop a       =   do { op <- pop; b <- pa; rest pa pop (op a b) } <|> return a  

char                ::  Char     -> Parser Char
char  c             =   satisfy (c ==)

nat                 ::  Parser Integer 
nat                 =   read <$> natStr
natStr              ::  Parser String
natStr              =   some (satisfy isDigit)

str                 ::  String   -> Parser String
str  ""             =   return []
str  (c:cs)         =   do { char c; str cs; return (c:cs)}

token               ::  Parser a -> Parser a 
token   p           =   do { a <- p; spaces ; return a }

reserved            ::  String   -> Parser String 
reserved s          =   token (str s) 

spaces              ::  Parser String
spaces              =   many $ oneOf " \n\r"

digit               ::  Parser Char
digit               =   satisfy isDigit

number              ::  Parser Int 
number              =   do { s<-str "-" <|> return []; cs<-some digit; return $ read(s++cs) }
num                 =   token number 

parens              :: Parser a -> Parser a
parens m            = do { reserved "("; spaces; n <- m; spaces; reserved ")"; return n }



data Expr
    = Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Lit Int
    deriving Show

eval    :: Expr -> Int
eval ex = case ex of 
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b
    Sub a b -> eval a - eval b
    Lit n   -> n


-- Backus-Naur Form 
-- number   = [ "-" ] digit { digit }.
-- digit    = "0" | "1" | ... | "8" | "9".
-- expr     = term      { addop term    }.
-- term     = factor    { mulop factor  }.
-- factor   = "(" expr ")" | number.
-- addop    = "+" | "-"
-- mulop    = "*"

int             ::  Parser Expr
int             =   num >>= return . Lit  

expr            ::  Parser Expr
expr            =   term `chainl1` addop 

term            ::  Parser Expr
term            =   factor `chainl1` mulop

factor          ::  Parser Expr
factor          =   int <|> parens expr

infixOp         ::  String -> (a->a->a) -> Parser(a->a->a)
infixOp x f     =   reserved x >> return f

addop           ::  Parser (Expr -> Expr -> Expr)
addop           =   (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop           ::  Parser (Expr -> Expr -> Expr)
mulop           =   infixOp "*" Mul

run     :: String   -> Expr
run     = runParser expr

main    :: IO ()
main    = forever $ do
    putStr "Calculator> "
    a <- getLine
    print $ eval $ run a
