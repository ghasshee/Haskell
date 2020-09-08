module Parser where 

import Data.Char
import Control.Monad
import Control.Applicative 



{----------- Parser MONAD --------------} 

data Parser token   = Parser ( String -> [(token, Rest)] ) 
parse psr str       = let Parser parse = psr in parse str 

runParser m s       =  case (parse m s) of 
    [(res,[])]  ->  res
    [(_,rest)]  ->  error "Parser didn't consume the entire stream."
    _           ->  error "Parser error."

swap (x,y) = (y,x) 
(...) = (.)(.)(.)

instance Functor Parser     where 
    fmap                    =   (...) Parser map_parse 
instance Applicative Parser where 
    pure                    =   return 
    (<*>)                   =   (...) Parser app_parse
instance Monad Parser       where 
    return                  =   ( . ) Parser just_parse  
    (>>=)                   =   (...) Parser bind_parse
instance MonadPlus Parser   where 
    mzero                   =         Parser null_parse 
    mplus                   =   (...) Parser cat_parse
instance Alternative Parser where 
    empty                   =   mzero 
    (<|>)                   =   (...) Parser option_parse

map_parse f psr s           =   fmap (\(x,y)->(f x,y))(parse psr s)
app_parse psrf psr s        =   [(f x,rrs)|(f,rs)<-parse psrf s, (x,rrs)<-parse psr rs]
just_parse x s              =   [(x,s)]
bind_parse psr psrgen s     =   concat . map (\(token,rest) -> parse(psrgen token)rest) $ parse psr s
cat_parse psr psr' s        =   parse psr s ++ parse psr' s 
null_parse  _               =   []
option_parse psr psr' s     =   case parse psr s of 
                                []      -> parse psr' s 
                                x       -> x


{----------------- Lexer --------------------} 

type Lexer                  =   Parser Char 
type Lex                    =   Char 
type Rest                   =   String 

lexer                       =   Parser lex                          :: Lexer 
    where   lex ""              =   []                                 
            lex (chr:rest)      =   [(chr,rest)]          
filtered p chr              =   if p chr then return chr else empty :: Lexer 
filtered_lexer p            =   filtered p =<< lexer                :: Lexer 
oneOf str                   =   filtered_lexer (flip elem str)      :: Lexer 
(<--) str                   =   oneOf str                           :: Lexer 
digit_lexer                 =   filtered_lexer  isDigit             :: Lexer 
char  c                     =   filtered_lexer (c ==)               :: Lexer 

cons psr                    =   fmap (:) psr                   
list psr                    =   cons psr <*> list psr  <|>  pure[]  :: Parser String 
nelist psr                  =   cons psr <*> list psr               :: Parser String  
digits                      =   nelist digit_lexer                  :: Parser String
reserved s                  =   token (string s)                    :: Parser String 
spaces                      =   list $ (<--) [' ','\n','\r']        :: Parser String 
string  ""                  =   return []                           :: Parser String 
string (c:cs)               =   do{char c;string cs;return (c:cs)}  :: Parser String 
uminus                      =   string "-"                          :: Parser String 

integer                     =   token $ do{ 
                                    sig <- uminus <|> return[] ; 
                                    num <- digits; 
                                    return $ read $ sig++num }      :: Parser Int 
digit                       =   read <$> digits                     :: Parser Int 

token   psr                 =   do { a<-psr; spaces; return a}
parens  psr                 =   do { reserved"("; spaces; a<-psr; spaces; reserved")"; return a }



{------------------ Syntax --------------------} 

data Expr   = Add Expr Expr
            | Mul Expr Expr
            | Sub Expr Expr
            | Int Int           deriving Show




{------------------ Eval  --------------------} 

eval        :: Expr -> Int
eval ex     = case ex of 
    Add a b     -> eval a + eval b
    Mul a b     -> eval a * eval b
    Sub a b     -> eval a - eval b
    Int n       -> n



{------------------ Parser --------------------}


chainl1             ::  Parser a -> Parser (a -> a -> a) -> Parser a 
p `chainl1` op      =   do 
    a <- p
    rest p op a  where 
        rest pa pop a   =   do { op <- pop; b <- pa; rest pa pop (op a b) } <|> return a
(%%) = chainl1 


expr            =   term   %% addop                     :: Parser Expr 
term            =   factor %% mulop                     :: Parser Expr 
factor          =   int 
                <|> parens expr                         :: Parser Expr 
int             =   return . Int =<< integer            :: Parser Expr 

infixOp x f     =   reserved x >> return f              :: Parser(Expr->Expr->Expr) 
addop           =   infixOp "+" Add 
                <|> infixOp "-" Sub                     :: Parser(Expr->Expr->Expr) 
mulop           =   infixOp "*" Mul                     :: Parser(Expr->Expr->Expr) 


{----------------- Main  ----------------------} 

run     :: String   -> Expr
run     = runParser expr


main    :: IO ()
main    = forever $ do
    putStr "Haskell Calculator >>> "
    a <- getLine
    print $ eval $ run a
