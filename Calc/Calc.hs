module Main where 

import Data.Char
import Control.Monad
import Control.Applicative hiding (somer, many) 
import Core 
import System.IO


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

expr            =   term    `infix_chainr1` add_sub        :: Parser Expr 
term            =   factor  `infix_chainr1` mul            :: Parser Expr 
factor          =   int 
                <|> parens expr                         :: Parser Expr 
int             =   (return . Int) =<< integer          :: Parser Expr 

binop str expr  =   do { _ <- reserved str ; return expr } 
add_sub         =   binop "+" Add 
                <|> binop "-" Sub                     :: Parser(Expr->Expr->Expr) 
mul             =   binop "*" Mul                     :: Parser(Expr->Expr->Expr) 

{----------------- Main  ----------------------} 

run     :: String   -> Expr
run     = runParser expr


main    :: IO ()
main    = forever $ do
    putStr "Haskell Calculator >>> "
    hFlush stdout
    a <- getLine
    print $ eval $ run a
