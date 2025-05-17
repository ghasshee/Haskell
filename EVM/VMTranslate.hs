module VMTranslate where 

import Prelude hiding (EQ,GT,LT,not,and,or,lex) 
import Parser
import Lexer 

copy i s = (.)(.)(.) concat replicate i ( s ++ "\n" )


------- PUSH & POP  ----------
-------    between the D-Register and the STACK -------
setD_static f i =   "@" ++ f ++ "." ++ show i  ++ "\n" ++
                    "D=M"               ++ "\n"

setD_const n    =   "@" ++ show n       ++ "\n" ++ 
                    "D=A"               ++ "\n" 

setD seg 0      =   "@" ++ seg          ++ "\n" ++         
                    "A=M"               ++ "\n" ++ 
                    "D=M"               ++ "\n" 
setD seg i      =   "@" ++ seg          ++ "\n" ++
                    "A=M+1"             ++ "\n" ++
                    copy (i-1) "A=A+1"          ++
                    "D=M"               ++ "\n" 

getD_push       =   "@SP"               ++ "\n" ++
                    "AM=M+1"            ++ "\n" ++
                    "A=A-1"             ++ "\n" ++ 
                    "M=D"               ++ "\n" 

pop_setD        =   "@SP"               ++ "\n" ++ 
                    "AM=M-1"            ++ "\n" ++ 
                    "D=M"               ++ "\n"

getD seg 0      =   "@" ++ seg          ++ "\n" ++         
                    "A=M"               ++ "\n" ++ 
                    "M=D"               ++ "\n" 
getD seg i      =   "@" ++ seg          ++ "\n" ++
                    "A=M+1"             ++ "\n" ++
                    copy (i-1) "A=A+1"          ++
                    "M=D"               ++ "\n" 

getD_static f i =   "@" ++ f ++ "." ++ show i ++ "\n" ++
                    "M=D"               ++ "\n"

------ OPERATORS -------
add             =   "@SP"               ++ "\n" ++ 
                    "AM=M-1"            ++ "\n" ++
                    "D=M"               ++ "\n" ++ 
                    "A=A-1"             ++ "\n" ++ 
                    "M=D+M"             ++ "\n" 

sub             =   "@SP"               ++ "\n" ++ 
                    "AM=M-1"            ++ "\n" ++
                    "D=M"               ++ "\n" ++ 
                    "A=A-1"             ++ "\n" ++ 
                    "M=M-D"             ++ "\n" 

and             =   "@SP"               ++ "\n" ++ 
                    "AM=M-1"            ++ "\n" ++
                    "D=M"               ++ "\n" ++ 
                    "A=A-1"             ++ "\n" ++ 
                    "M=D&M"             ++ "\n" 

or              =   "@SP"               ++ "\n" ++ 
                    "AM=M-1"            ++ "\n" ++
                    "D=M"               ++ "\n" ++ 
                    "A=A-1"             ++ "\n" ++ 
                    "M=D|M"             ++ "\n" 

eq i            =   "@SP"               ++ "\n" ++ 
                    "AM=M-1"            ++ "\n" ++
                    "D=M"               ++ "\n" ++ 
                    "A=A-1"             ++ "\n" ++ 
                    "D=M-D"             ++ "\n" ++
                    "M=0"               ++ "\n" ++
                    "@END_EQ" ++ show i ++ "\n" ++
                    "D;JNE"             ++ "\n" ++     
                    "@SP"               ++ "\n" ++ 
                    "A=M-1"             ++ "\n" ++     
                    "M=-1"              ++ "\n" ++  
                    "(END_EQ" ++ show i ++ ")\n"

neq i           =   "@SP"               ++ "\n" ++ 
                    "AM=M-1"            ++ "\n" ++
                    "D=M"               ++ "\n" ++ 
                    "A=A-1"             ++ "\n" ++ 
                    "D=M-D"             ++ "\n" ++
                    "M=0"               ++ "\n" ++
                    "@END_NEQ"++ show i ++ "\n" ++
                    "D;JEQ"             ++ "\n" ++     
                    "@SP"               ++ "\n" ++ 
                    "A=M-1"             ++ "\n" ++     
                    "M=-1"              ++ "\n" ++  
                    "(END_NEQ"++ show i ++ ")\n"

lt i            =   "@SP"               ++ "\n" ++ 
                    "AM=M-1"            ++ "\n" ++
                    "D=M"               ++ "\n" ++ 
                    "A=A-1"             ++ "\n" ++ 
                    "D=M-D"             ++ "\n" ++
                    "M=0"               ++ "\n" ++
                    "@END_LT" ++ show i ++ "\n" ++
                    "D;JGE"             ++ "\n" ++     
                    "@SP"               ++ "\n" ++ 
                    "A=M-1"             ++ "\n" ++     
                    "M=-1"              ++ "\n" ++  
                    "(END_LT" ++ show i ++ ")\n"

gt i            =   "@SP"               ++ "\n" ++ 
                    "AM=M-1"            ++ "\n" ++
                    "D=M"               ++ "\n" ++ 
                    "A=A-1"             ++ "\n" ++ 
                    "D=M-D"             ++ "\n" ++
                    "M=0"               ++ "\n" ++
                    "@END_GT" ++ show i ++ "\n" ++
                    "D;JLE"             ++ "\n" ++     
                    "@SP"               ++ "\n" ++ 
                    "A=M-1"             ++ "\n" ++     
                    "M=-1"              ++ "\n" ++  
                    "(END_GT" ++ show i ++ ")\n"

neg             =   "@SP"               ++ "\n" ++   
                    "A=M"               ++ "\n" ++ 
                    "M=-M"              ++ "\n"

not             =   "@SP"               ++ "\n" ++   
                    "A=M"               ++ "\n" ++ 
                    "M=!M"              ++ "\n"




----- VMTranslate FUNCTION ----------
translate :: Int -> String -> Term -> String 
translate k f t   = case t of 
    Push Const      n   -> setD_const       n ++ getD_push
    Push Static     i   -> setD_static   f  i ++ getD_push
    Push Local      i   -> setD "LCL"       i ++ getD_push 
    Push Arg        i   -> setD "ARG"       i ++ getD_push 
    Push This       i   -> setD "THIS"      i ++ getD_push 
    Push That       i   -> setD "THAT"      i ++ getD_push 
    Push Pointer    i   -> setD "POINTER"   i ++ getD_push 
    Push Temp       i   -> setD "TEMP"      i ++ getD_push 
    Pop  Const      n   -> error "Cannot place pop item" 
    Pop  Static     i   -> pop_setD ++ getD_static   f  i 
    Pop  Local      i   -> pop_setD ++ getD "LCL"       i 
    Pop  Arg        i   -> pop_setD ++ getD "ARG"       i 
    Pop  This       i   -> pop_setD ++ getD "THIS"      i 
    Pop  That       i   -> pop_setD ++ getD "THAT"      i 
    Pop  Pointer    i   -> pop_setD ++ getD "POINTER"   i
    Pop  Temp       i   -> pop_setD ++ getD "TEMP"      i
    Not                 -> not
    Neg                 -> neg
    Add                 -> add
    Sub                 -> sub 
    And                 -> and
    Or                  -> or 
    Eq                  -> eq k
    Lt                  -> lt k
    Gt                  -> gt k
    e                   -> error $ show e ++ " cannot be translate." 

