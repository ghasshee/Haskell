module Data.PrettyStub where

import Data.Json


data Doc    = ToBeDefined 
    deriving Show


string  ::  String  -> Doc
string  =   enclose '"' '"' . hcat . map oneChar  

text :: String -> Doc
text str    = undefined 

double :: Double -> Doc
double num  = undefined

(<>)    ::  Doc     -> Doc  -> Doc
a <> b  =   undefined

char    ::  Char    -> Doc
char c  = undefined

enclose = undefined 

hcat    ::  [Doc]   -> Doc
hcat xs =   undefined 

oneChar     ::  Char    -> Doc
oneChar c   =   case lookup c simpleEscapes of 
                    Just r                  -> text r  
                    Nothing | mustEscape c  -> hexEscape c
                            | otherwise     -> char c 
                where
                    mustEscape c    = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes   ::  [(Char, String)]
simpleEscapes   =   zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b    = (a, ['\\', b])


hexEscape = undefined

