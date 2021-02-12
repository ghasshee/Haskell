module Parser where 

import Prelude hiding (EQ,GT,LT,lex) 
import Lexer 

data Segment    = Arg
                | Local
                | Static
                | Const 
                | This 
                | That 
                | Pointer
                | Temp  deriving (Show) 

type Index      = Int 

data Term       = Pop  Segment Index
                | Push Segment Index
                | Add
                | Sub 
                | Or
                | And 
                | Neg
                | Eq
                | Lt
                | Gt
                | Not deriving (Show)  









parse_seg seg = case seg of 
      ARG       -> Arg
      LOCAL     -> Local
      STATIC    -> Static
      CONST     -> Const
      THIS      -> This
      THAT      -> That
      POINTER   -> Pointer
      TEMP      -> Temp

parse :: [Token] -> [Term] 
parse []      = []
parse (t:ts)  = case t of 
    POP             ->  let seg:(INT i):rest = ts in 
                        Pop (parse_seg seg) i : parse rest
    PUSH            ->  let seg:(INT i):rest = ts in 
                        Push (parse_seg seg) i : parse rest
    ADD             -> Add : parse ts 
    SUB             -> Sub : parse ts  
    OR              -> Or  : parse ts 
    AND             -> And : parse ts 
    NOT             -> Not : parse ts  
    NEG             -> Neg : parse ts 
    EQ              -> Eq  : parse ts 
    LT              -> Lt  : parse ts 
    GT              -> Gt  : parse ts
    _    -> error "parse error" 

