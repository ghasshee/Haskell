module Core where 

import Data.Tuple (swap)
import Control.Monad
import Control.Applicative 

-- parse :: String -> [(a,String)] -> Parser a 
newtype Parser a    =   Parser { parse :: String -> [(a,String)] }

runParser           ::  Parser a -> String -> a 
runParser m s       =   
    case (parse m s) of 
    [(res,[])]  ->  res
    [(_,rest)]  ->  error "Parser didn't consume the entire stream."
    _           ->  error "Parser error."

(...) = (.)(.)(.)

instance Functor Parser     where 
    fmap                    =   (...) Parser fmapParser 
instance Applicative Parser where 
    pure                    =   return 
    (<*>)                   =   (...) Parser concatParser 
instance Monad Parser       where 
    return                  =   ( . ) Parser unitParser  
    (>>=)                   =   (...) Parser bindParser 
instance MonadPlus Parser   where 
    mzero                   =         Parser nullParser 
    mplus                   =   (...) Parser combineParser
instance Alternative Parser where 
    empty                   =   mzero 
    (<|>)                   =   (...) Parser optionParser

fmapParser f p s        =   fmap (swap.fmap f.swap)(parse p s)
concatParser pf pa s    =   [(f a,rrs)|(f,rs)<-parse pf s, (a,rrs)<-parse pa rs]
unitParser x s          =   [(x,s)]
bindParser p pg s       =   concatMap (apply pg) (parse p s)
apply pg (a,s)          =   parse (pg a) s         -- pg : parser generator
combineParser p q s     =   parse p s ++ parse q s 
nullParser  s           =   []
optionParser p q s      =   case parse p s of 
                                []      -> parse q s 
                                x       -> x

-- satisfy checks whether the current char in the stream matches a given predicate 
satisfy                 ::  (Char -> Bool) -> Parser Char  --Predicate->Parser Char 
satisfy p               =   Parser itemParser >>= justUnit p

itemParser ""           =   []
itemParser (x:xs)       =   [(x,xs)]
justUnit p c            =   if p c    then return c     else empty

