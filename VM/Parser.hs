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

data Term       = Label String 
                | Goto String 
                | IfGoto String 
                | Call String Int
                | Fun String Int 
                | Ret
                | Pop  Segment Index
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
    LABEL           ->  let (STRING s):rest = ts in 
                        Label s : parse rest
    GOTO            ->  let (STRING s):rest = ts in 
                        Goto s : parse rest 
    IFGOTO          ->  let (STRING s):rest = ts in 
                        IfGoto s : parse rest 
    CALL            ->  let (STRING s):(INT i):rest = ts in 
                        Call s i : parse rest 
    FUN             ->  let (STRING s):(INT i):rest = ts in 
                        Fun s i : parse rest 
    RET             ->  Ret : parse ts  
    POP             ->  let seg:(INT i):rest = ts in 
                        Pop (parse_seg seg) i : parse rest
    PUSH            ->  let seg:(INT i):rest = ts in 
                        Push (parse_seg seg) i : parse rest
    ADD             ->  Add : parse ts 
    SUB             ->  Sub : parse ts  
    OR              ->  Or  : parse ts 
    AND             ->  And : parse ts 
    NOT             ->  Not : parse ts  
    NEG             ->  Neg : parse ts 
    EQ              ->  Eq  : parse ts 
    LT              ->  Lt  : parse ts 
    GT              ->  Gt  : parse ts
    _               -> error "parse error" 

