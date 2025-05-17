module Core where 

import qualified Data.ByteString.Char8 as B ( ByteString, empty , pack , unpack ) 
import Data.Attoparsec.ByteString.Char8  hiding (string, space) 

runParser m s       =  case ( parse (m <* endOfInput) s) of 
    Partial k    ->  ( case k (B.pack "") of
        Done d res  -> Just res
        _           -> Nothing ) 
    Done e  res  ->  Just res
    e            ->  error $ show e   

{----------------- Lexer Module --------------------} 

type Lexer                  =   Parser Char 

oneOf str                   =   satisfy (flip elem str)      :: Lexer 
cons psr                    =   fmap (:) psr                   
list                        =   many'
nelist                      =   many1
digits                      =   nelist digit                        
reserved s                  =   token (string s)                    
space                       =   oneOf [' ', '\t', '\r']
spaces                      =   list space              :: Parser [Char]                        
some_spaces                 =   nelist space            :: Parser [Char]             
newline                     =   nelist space            
string  ""                  =   return $  []                        
string (c:cs)               =   do{char c;string cs;return $ (c:cs)}   
uinteger                    =   do { r <- read <$> digits ; spaces ; return r }  :: Parser Int 
token   psr                 =   do { a<-psr; spaces; return a}
parens  psr                 =   do { reserved "(" ; spaces; a<-psr; spaces; reserved ")"; return a }


