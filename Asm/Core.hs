{-# LANGUAGE ScopedTypeVariables #-} 


module Core where 

import qualified Data.ByteString.Char8 as B ( ByteString, empty , pack , unpack ) 
import Data.Char
import Control.Monad
import Control.Applicative hiding (some, many) 
-- import Text.ParserCombinators.Parsec (eof) 

import Data.Attoparsec.ByteString.Char8  hiding (string, space) 
import qualified Data.Attoparsec.Internal.Types as A 


{----------- Parser MONAD --------------} 




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


runParser m s       =  case ( parse (m <* endOfInput) s) of 
    Partial k    ->  ( case k (B.pack "") of
        Done d res  -> {-- error $ "Partial parse complete :: " ++ "Done : " ++ show d ++ ":" ++ show res --}  Just res
        _           -> Nothing ) 
    Done e  res  ->  Just res
    e            ->  error $ show e   

{----------------- Lexer Module --------------------} 

type Lexer                  =   Parser Char 


lexer = anyChar 

-- filtered p chr              =   if p chr then return chr else mzero :: Lexer 
-- filtered_lexer p            =   filtered p =<< lexer                :: Lexer 
oneOf str                   =   satisfy (flip elem str)      :: Lexer 
-- digit                       =   filtered_lexer  isDigit             :: Lexer 
letter                      =   satisfy isLetter             :: Lexer 


cons psr                    =   fmap (:) psr                   
-- list psr                    =   cons psr <*> list psr  <|>  mzero 
-- nelist psr                  =   cons psr <*> list psr             
-- (some,many)                 =   (list,nelist)   
list = many'
nelist = many1



digits                      =   nelist digit                        
reserved s                  =   token (string s)                    
space                       =   oneOf [' ', '\t', '\r']
spaces                      =   list space              :: Parser [Char]                        
some_spaces                 =   nelist space            :: Parser [Char]             
newline                     =   nelist space            
uminus                      =   char '-'                           

string  ""                  =   return $  []                        
string (c:cs)               =   do{char c;string cs;return $ (c:cs)}   

{--
integer                     =   token $ do{ 
                                    sig <- uminus <|> mzero ; 
                                    num <- digits; 
                                    return $ read $ sig :  num }      :: Parser Int --} 
uinteger                    =   do { r <- read <$> digits ; spaces ; return r }  :: Parser Int 

token   psr                 =   do { a<-psr; spaces; return a}
parens  psr                 =   do { reserved "(" ; spaces; a<-psr; spaces; reserved ")"; return a }



-- eof = Text.ParserCombinators.Parsec.eof 
-- eof                         =   reserved [Data.Char.chr 0x1A] 


(<<) a b = (>>) b a 
