module Main where 

import Syntax
import Lexer
import Parser

import Prelude hiding (abs, lex, True, False)
import Data.Char
import Control.Monad
import Control.Applicative hiding (some,many) 
import System.IO (hFlush, stdout) 



-- type Context = [String] 


{------------------ Eval  --------------------} 


tmSubstTop t t2         =   tmShift (-1) (tmSubst 0 (tmShift 1 t) t2)
tmSubst  j t t2         =   walk (tmSubstOnVar j t t2) 0 t2
tmSubstOnVar j t t2     =   \c i -> if i==j+c then tmShift c t else TmVar i 

tmShift d               =   tmShiftAbove d 0 
tmShiftAbove d          =   walk (tmShiftOnVar d) 
tmShiftOnVar d          =   \c i -> if i>=c then TmVar (d+i) else TmVar i 

walk onVar c t          =   case t of 
    TmApp t1 t2             -> TmApp (walk onVar c t1) (walk onVar c t2) 
    TmAbs ty t              -> TmAbs ty (walk onVar (c+1) t) 
    TmVar i                 -> onVar c i 
    TmZero                  -> TmZero 



eval ctx t              =   case eval1 ctx t of 
    Nothing                 -> t 
    Just t'                 -> eval ctx t' 
     

eval1                   ::  Context -> Term -> Maybe Term 
eval1 ctx t             =   case t of 
    TmApp (TmAbs ty t') t2  -> Just $ tmSubstTop t2 t' 
    TmApp t1 t2             -> case eval1 ctx =<< Just t1 of 
        Nothing                 -> Nothing 
        Just t1'                -> Just $ TmApp t1' t2 
    TmAbs ty t'             -> return . TmAbs ty =<< eval1 ctx t'   
    TmVar i                 -> Nothing  
    TmZero                  -> Nothing 


{----------------- Main  ----------------------} 

run         ::  String   -> Term
run         =   flip parse [] . lex 


main        ::  IO ()
main        =   forever $ do
    putStr "λ> "
    hFlush stdout
    a <- getLine; 
    let t = run a ; 
    putStrLn $ show  t  
    putStrLn " ---------- evaluation ----------- " 
    let e = eval [] t;  
    putStrLn $ show e 



