module Core where 

import Data.Char
import Control.Monad
import Control.Applicative hiding (some, many) 
import Text.ParserCombinators.Parsec (eof) 


{----------- Parser MONAD --------------} 

data Parser token   = Parser ( String -> [(token, Rest)] ) 
parse psr str       = let Parser parse = psr in parse str 

runParser m s       =  case (parse m s) of 
    [(res,[])]  ->  res
    [(_,rest)]  ->  error $ ( show rest :: String) 
    e           ->  error $ show e   

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


{------------------ Parser Module --------------------}

--  Chains :: Parser t -> Parser(t->t->t) -> Parser t   
--  are parsers of e.g.     :  a + b + c + ... + y + z
--  more precisely; 
--      infix_chain_left    :  (((a + b) + c) + ... + y) + z 
--
--      infix_chain_right   :  a + (b + (c + ... + (y + z)))
--
--      prefix_chain_left   :  +(+ ... (+(+ a b) c) ... y) z 
--             ... 

prefix_chainl1 psr psrop    = do{ 
        op  <-  psrop; 
        a   <-  prefix_chainl1 psr psrop; 
        b   <-  psr; 
        return (op a b)} <|> psr   

prefix_chainr1 psr psrop    =   do{ 
        op  <-  psrop; 
        a   <-  psr; 
        b   <-  prefix_chainr1 psr psrop; 
        return (op a b)} <|> psr   

infix_chainl1 psr psrop     =   psr >>= rest psr psrop  
    where rest psr' psrop a =   do{     
        op  <-  psrop; 
        b   <-  psr'; 
        rest psr' psrop (op a b) } <|> return a                                    

infix_chainr1 psr psrop     =   do{
        a   <-  psr;
        op  <-  psrop; 
        b   <-  infix_chainr1 psr psrop;
        return (op a b)} <|> psr 

postfix_chainl1 psr psrop   =   psr >>= rest psr psrop
    where rest psr' psrop a =   do{ 
        b   <-  psr' ; 
        op  <-  psrop; 
        rest psr' psrop (op a b) } <|> return a

postfix_chainr1 psr psrop   =   do{ 
        a   <-  psr ; 
        b   <-  postfix_chainr1 psr psrop ;
        op  <-  psrop ;
        return (op a b) } <|> psr




sepBy1 p sep    = do { 
    x   <- p ; 
    xs  <- list (sep >> p) ; 
    return (x:xs)   
    } 


{----------------- Lexer Module --------------------} 

type Lexer                  =   Parser Char 
type Lex                    =   Char 
type Rest                   =   String 

lexer                       =   Parser lex                          :: Lexer 
    where   lex ""              =   []                                 
            lex (chr:rest)      =   [(chr,rest)]          

filtered p chr              =   if p chr then return chr else empty :: Lexer 
filtered_lexer p            =   filtered p =<< lexer                :: Lexer 
oneOf str                   =   filtered_lexer (flip elem str)      :: Lexer 
digit                       =   filtered_lexer  isDigit             :: Lexer 
char  c                     =   filtered_lexer (c ==)               :: Lexer 
letter                      =   filtered_lexer isLetter             :: Lexer 


cons psr                    =   fmap (:) psr                   
list psr                    =   cons psr <*> list psr  <|>  pure[]  :: Parser String 
nelist psr                  =   cons psr <*> list psr               :: Parser String  
(some,many)                 =   (list,nelist)   


digits                      =   nelist digit                        :: Parser String
reserved s                  =   token (string s)                    :: Parser String 
space                       =   oneOf " \t\n" 
spaces                      =   list space                          :: Parser String 
some_spaces                 =   nelist space                        :: Parser String 
newline                     =   nelist $ oneOf " \t\r\n"            :: Parser String 
string  ""                  =   return []                           :: Parser String 
string (c:cs)               =   do{char c;string cs;return (c:cs)}  :: Parser String 
uminus                      =   string "-"                          :: Parser String 

integer                     =   token $ do{ 
                                    sig <- uminus <|> return[] ; 
                                    num <- digits; 
                                    return $ read $ sig++num }      :: Parser Int 
uinteger                    =   read <$> digits                     :: Parser Int 

token   psr                 =   do { a<-psr; spaces; return a}
parens  psr                 =   do { reserved"("; spaces; a<-psr; spaces; reserved")"; return a }


initletter                  =   letter <|> oneOf "_"
tailletter                  =   list ( digit <|> initletter )         
ident                       =   do{ i <- initletter; t <- tailletter; return (i:t) }  :: Parser String 

-- eof = Text.ParserCombinators.Parsec.eof 
eof                         =   reserved [Data.Char.chr 0x1A] 


(<<) a b = (>>) b a 
