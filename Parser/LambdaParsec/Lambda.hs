module Main where 

import Prelude hiding (abs) 
import Data.Char
import Control.Monad
import Control.Applicative hiding (some,many) 
import Core 
import System.IO (hFlush, stdout) 


{------------------ Syntax --------------------} 

type Context = [String] 

data Term   = TmAbs Term 
            | TmApp Term Term 
            | TmVar Int           deriving (Show, Eq)

str2index = loop 0      where   
    loop :: Int -> String -> Context -> Int 
    loop n str []        =   error "Variable Lookup Error" 
    loop n str (x:xs)    =   if x == str then n else loop(n+1) str xs  

index2str i list = (!!) list i  


{---------------- Parser ---------------------} 

line        = do {spaces; t <- ( term <*> pure [] ); spaces;  return t }    :: Parser Term 

term        = infix_chainl1 nonapp (spaces >> return (\t1 t2 ctx -> TmApp(t1 ctx)(t2 ctx)))
nonapp      =   var  
            <|> abs term 
            <|> parens term 

abs term    =   do { 
    spaces; 
    reserved "\\";
    list space; 
    str     <- ident;
    list space; 
    reserved "."; 
    spaces; 
    t       <- term ; 
    spaces; 
    return $ \ctx -> (TmAbs (t(str:ctx)))  }  

var             =  parens term <|> do{
    str <- ident; 
    let _ = print "var is parsed" in 
    return $ \ctx -> TmVar (str2index str ctx) }


{------------------ Eval  --------------------} 


tmSubstTop t t2         =   tmShift (-1) (tmSubst 0 (tmShift 1 t) t2)
tmSubst  j t t2         =   walk (tmSubstOnVar j t t2) 0 t2
tmSubstOnVar j t t2     =   \c i -> if i==j+c then tmShift c t else TmVar i 

tmShift d               =   tmShiftAbove d 0 
tmShiftAbove d          =   walk (tmShiftOnVar d) 
tmShiftOnVar d          =   \c i -> if i>=c then TmVar (d+i) else TmVar i 

walk onVar c t          = case t of 
    TmApp t1 t2             -> TmApp (walk onVar c t1) (walk onVar c t2) 
    TmAbs t                 -> TmAbs (walk onVar (c+1) t) 
    TmVar i                 -> onVar c i 



eval ctx t = case eval1 ctx t of 
    Nothing     -> t 
    Just t'     -> eval ctx t' 
     

eval1            :: Context -> Term -> Maybe Term 
eval1 ctx t         = case t of 
    TmAbs t'                    -> eval1 ctx =<< Just t'  
    TmApp (TmAbs t') t2         -> Just $ tmSubstTop t2 t' 
    TmApp t1 t2                 -> case eval1 ctx =<< Just t1 of 
        Nothing     -> Nothing 
        Just t1'    -> Just $ TmApp t1' t2 
    TmVar i                     -> Nothing  


{----------------- Main  ----------------------} 

run     :: String   -> Term
run     = runParser $ line


main    :: IO ()
main    = forever $ do
    putStr "λ >> "
    hFlush stdout
    a <- getLine; 
    let t = run a ; 
    putStrLn $ show  t  
    putStrLn " ---------- evaluation ----------- " 
    let e = eval [] t;  
    putStrLn $ show e 



