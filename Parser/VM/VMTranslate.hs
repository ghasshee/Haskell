module VMTranslate where 

import Prelude hiding (EQ,GT,LT,not,and,or,lex) 
import Parser
import Lexer 

import Control.Monad.State 

type ST = State Int 

copy i s = (.)(.)(.) concat replicate i ( s ++ "\n" )


------- PUSH & POP  ----------
-------    between the D-Register and the STACK -------
setD_static f i =   "@" ++ f ++ "." ++ show i  ++ "\n" ++
                    "D=M"               ++ "\n"

setD_const n    =   "@" ++ show n       ++ "\n" ++ 
                    "D=A"               ++ "\n" 

setD_pointer 0  =   "@THIS"             ++ "\n" ++
                    "D=M"               ++ "\n" 

setD_pointer 1  =   "@THAT"             ++ "\n" ++
                    "D=M"               ++ "\n" 

setD_temp i     =   "@R" ++ show(i+5)   ++ "\n" ++ 
                    "D=M"               ++ "\n" 

setD seg 0      =   "@" ++ seg          ++ "\n" ++         
                    "A=M"               ++ "\n" ++ 
                    "D=M"               ++ "\n" 
setD seg i      =   "@" ++ seg          ++ "\n" ++
                    "A=M+1"             ++ "\n" ++
                    copy (i-1) "A=A+1"          ++
                    "D=M"               ++ "\n" 

pushD           =   "@SP"               ++ "\n" ++
                    "AM=M+1"            ++ "\n" ++
                    "A=A-1"             ++ "\n" ++ 
                    "M=D"               ++ "\n" 

popD            =   "@SP"               ++ "\n" ++ 
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

getD_pointer 0  =   "@THIS"             ++ "\n" ++
                    "M=D"               ++ "\n" 
getD_pointer 1  =   "@THAT"             ++ "\n" ++
                    "M=D"               ++ "\n" 
getD_pointer _  =  error "Pointer Error" 


getD_temp i     =   "@R" ++ show (i+5)  ++ "\n" ++
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
                    "A=M-1"             ++ "\n" ++ 
                    "M=-M"              ++ "\n"

not             =   "@SP"               ++ "\n" ++   
                    "A=M-1"             ++ "\n" ++ 
                    "M=!M"              ++ "\n"

label s         =   "(" ++ s ++ ")"     ++ "\n" 

goto label      =   "@" ++ label        ++ "\n" ++
                    "0;JMP"             ++ "\n" 

ifgoto label    =   popD                        ++ 
                    "@" ++ label        ++ "\n" ++
                    "D;JNE"             ++ "\n" 



------------- CALL -----------------
save f n        =   "@" ++ show n       ++ "\n" ++
                    "D=A"               ++ "\n" ++
                    "@R13"              ++ "\n" ++ 
                    "M=D"               ++ "\n" ++
                    "@" ++      f       ++ "\n" ++
                    "D=A"               ++ "\n" ++ 
                    "@R14"              ++ "\n" ++
                    "M=D"               ++ "\n" 

setD_RET_ADDR l =   "@RET_ADDR"++show l ++ "\n" ++
                    "D=A"               ++ "\n" 
push_RET_ADDR   =   "@SP"               ++ "\n" ++
                    "A=M"               ++ "\n" ++ 
                    "M=D"               ++ "\n" 

    ---------------------------------
    --  pushD' differs from pushD  --
    --                             --
    --   BEFORE   --->   AFTER     --
    --                             --
    --                   +----+    -- 
    --               SP->| D  |    --
    --      +----+       +----+    --
    --  SP->| a  |       | a  |    --
    ---------------------------------                                                        
setD' str       =   "@" ++ str          ++ "\n" ++       
                    "D=M"               ++ "\n"         
pushD'          =   "@SP"               ++ "\n" ++      
                    "AM=M+1"            ++ "\n" ++      
                    "M=D"               ++ "\n"         
push' str       =   setD' str ++ pushD'                 
                                                        
repos_ARG       =   "@4"                ++ "\n" ++                     
                    "D=A"               ++ "\n" ++
                    "@R13"              ++ "\n" ++ 
                    "D=D+M"             ++ "\n" ++
                    "@SP"               ++ "\n" ++
                    "D=M-D"             ++ "\n" ++
                    "@ARG"              ++ "\n" ++
                    "M=D"               ++ "\n" 

repos_LCL       =   "@SP"               ++ "\n" ++
                    "MD=M+1"            ++ "\n" ++
                    "@LCL"              ++ "\n" ++
                    "M=D"               ++ "\n" 

goto_f          =   "@R14"              ++ "\n" ++
                    "A=M"               ++ "\n" ++
                    "0;JMP"             ++ "\n" 

call f n l      =   save f n                    ++ 
                    setD_RET_ADDR l             ++
                    push_RET_ADDR               ++ 
                    push' "LCL"                 ++
                    push' "ARG"                 ++
                    push' "THIS"                ++
                    push' "THAT"                ++ 
                    repos_ARG                   ++
                    repos_LCL                   ++ 
                    goto_f                      ++      
                    label("RET_ADDR" ++ show l) 

sysinit         =   save "Sys.init" 0 ++
                    setD_RET_ADDR 0             ++
                    push_RET_ADDR               ++ 
                    push' "LCL"                 ++
                    push' "ARG"                 ++
                    push' "THIS"                ++
                    push' "THAT"                ++ 
                    repos_ARG ++
                    repos_LCL ++ 
                    "@Sys.init"         ++ "\n" ++
                    "0;JMP"             ++ "\n" ++
                    "(RET_ADDR0)"       ++ "\n" 

sysinit'        =   save "Sys.init" 0   ++
                    "@SP"               ++ "\n" ++
                    "D=M"               ++ "\n" ++ 
                    "@LCL"              ++ "\n" ++
                    "M=D"               ++ "\n" ++ 
                    "@Sys.init"         ++ "\n" ++
                    "0;JMP"             ++ "\n" 

--------- FUNCTION -------------
fun f 0         =   label f 
fun f k         =   label f                     ++
                    "@SP"               ++ "\n" ++ 
                    "A=M"               ++ "\n" ++
                    "M=0"               ++ "\n" ++
                    copy (k-1)("AD=A+1" ++ "\n" ++
                               "M=0")           ++ 
                    "@SP"               ++ "\n" ++
                    "M=D+1"             ++ "\n" 

--------- RETURN --------------
getRET_ADDR     =   "@5"                ++ "\n" ++
                    "D=A"               ++ "\n" ++
                    "@LCL"              ++ "\n" ++ 
                    "A=M-D"             ++ "\n" ++
                    "D=M"               ++ "\n" ++
                    "@R13"              ++ "\n" ++
                    "M=D"               ++ "\n" 

setRET          =   "@SP"               ++ "\n" ++
                    "AM=M-1"            ++ "\n" ++
                    "D=M"               ++ "\n" ++
                    "@ARG"              ++ "\n" ++
                    "A=M"               ++ "\n" ++
                    "M=D"               ++ "\n" 

setSP           =   "D=A"               ++ "\n" ++
                    "@SP"               ++ "\n" ++
                    "M=D+1"             ++ "\n" 

restoreTHAT     =   "@LCL"              ++ "\n" ++    
                    "D=M"               ++ "\n" ++    
                    "@R14"              ++ "\n" ++
                    "AM=D-1"            ++ "\n" ++
                    "D=M"               ++ "\n" ++
                    "@THAT"             ++ "\n" ++
                    "M=D"               ++ "\n" 

restore str     =   "@R14"              ++ "\n" ++    
                    "AM=M-1"            ++ "\n" ++        
                    "D=M"               ++ "\n" ++    
                    "@" ++ str          ++ "\n" ++
                    "M=D"               ++ "\n"    

gotoRET_ADDR    =   "@R13"              ++ "\n" ++
                    "A=M"               ++ "\n" ++
                    "0;JMP"             ++ "\n" 

ret             =   getRET_ADDR                 ++
                    setRET                      ++
                    setSP                       ++
                    restoreTHAT                 ++
                    restore "THIS"              ++ 
                    restore "ARG"               ++
                    restore "LCL"               ++
                    gotoRET_ADDR 


init            =   "@256"              ++ "\n" ++
                    "D=A"               ++ "\n" ++
                    "@SP"               ++ "\n" ++
                    "M=D"               ++ "\n" ++
                    sysinit 
-- init = "" 
----- VMTranslate FUNCTION ----------
translate :: String -> Term -> State Int String 
translate f t      = case t of 
    Label s             -> return $ label s  
    Goto s              -> return $ goto s 
    IfGoto s            -> return $ ifgoto s
    Call f n            -> state (\l -> (call f n l, l+1)) 
    Fun f k             -> return $ fun f k 
    Ret                 -> return $ ret
    Push Const      n   -> return $ setD_const       n ++ pushD
    Push Static     i   -> return $ setD_static   f  i ++ pushD
    Push Pointer    i   -> return $ setD_pointer     i ++ pushD 
    Push Local      i   -> return $ setD "LCL"       i ++ pushD 
    Push Arg        i   -> return $ setD "ARG"       i ++ pushD 
    Push This       i   -> return $ setD "THIS"      i ++ pushD 
    Push That       i   -> return $ setD "THAT"      i ++ pushD 
    Push Temp       i   -> return $ setD_temp        i ++ pushD 
    Pop  Const      n   -> return $ error "Cannot place pop item" 
    Pop  Static     i   -> return $ popD ++ getD_static   f  i 
    Pop  Pointer    i   -> return $ popD ++ getD_pointer     i
    Pop  Local      i   -> return $ popD ++ getD "LCL"       i 
    Pop  Arg        i   -> return $ popD ++ getD "ARG"       i 
    Pop  This       i   -> return $ popD ++ getD "THIS"      i 
    Pop  That       i   -> return $ popD ++ getD "THAT"      i 
    Pop  Temp       i   -> return $ popD ++ getD_temp        i
    Not                 -> return $ not
    Neg                 -> return $ neg
    Add                 -> return $ add
    Sub                 -> return $ sub 
    And                 -> return $ and
    Or                  -> return $ or 
    Eq                  -> state (\l -> (eq l, l+1))
    Lt                  -> state (\l -> (lt l, l+1))
    Gt                  -> state (\l -> (gt l, l+1))

