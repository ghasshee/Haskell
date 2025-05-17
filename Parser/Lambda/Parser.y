{
module Parser where 
 
import Syntax 
import Lexer 

} 

%name parse
%tokentype  { Token }
%error      { parseError } 
%token 
lam         { LAMBDA            }
pi          { PI                } 
x           { LCID $$           } 
'.'         { DOT               } 
ty          { UCID $$           } 
':'         { COLON             } 
'('         { LPAREN            }
')'         { RPAREN            }
univ        { UNIV              } 
i           { INT $$            } 

help        { HELP      }
context     { CONTEXT   } 
parameter   { PARAMETER } 
definition  { DEFINITION}
str         { STRING $$ }
':='        { COLONEQ   } 
check       { CHECK     } 
eval        { EVAL      } 


    
%%

Repl : {}
     | Repl Sentence {}

Sentence    : help '.'                      {}
            | context '.'                   {}
            | parameter str ':' Tm '.'      {}
            | definition str def Tm '.'     {}
            | check Tm '.'                  {}
            | eval  Tm '.'                  {}




Tm      :   lam x ':' Tm '.' Tm         { \c -> Lam $2 ($4 c)($6(addVar c $2))      } 
        |   pi  x ':' Tm '.' Tm         { \c -> Pi  $2 ($4 c)($6(addVar c $2))      } 
        |   AppTm                       { $1                                        } 

AppTm   :   ATm                         { $1                                        } 
        |   AppTm ATm                   { \c -> App ($1 c)($2 c)                    } 

ATm     :   '(' Tm ')'                  { $2                                        }
        |   x                           { \c -> Var $ getVar c $1                   } 
        |   univ i                      { \c -> Univ $2                             } 



{
parseError _ = error "!Parse Error" 


type Var = String 

getVar              :: Context -> Var -> Int 
getVar [] str       = error $ "cannot find " ++ str ++ " in context"
getVar (x:xs) str   = if fst x == str  then 0 else 1 + (getVar xs str)

addVar ctx str      = (str,NameBind):ctx 

}




