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
succ        { SUCC              }
pred        { PRED              } 
zero        { ZERO              } 
true        { TRUE              } 
false       { FALSE             } 
if          { IF                } 
then        { THEN              } 
else        { ELSE              } 
all         { ALL               } 
exists      { EXISTS            }
x           { LCID $$           } 
'.'         { DOT               } 
ty          { UCID $$           } 
':'         { COLON             } 
'('         { LPAREN            }
')'         { RPAREN            }
type        { TYPE $$           } 
set         { SET               } 
prop        { PROP              } 
truth       { TRUTH             } 
falsity     { FALSITY           }
nat         { NAT               } 
bool        { BOOL              } 

    
%%

Tm      :   lam x ':' Ty '.' Tm         { \c -> TmAbs ($4 c)($6(addVar c $2))       } 
        |   if Tm then Tm else Tm       { \c -> TmIf ($2 c)($4 c)($6 c)             } 
        |   AppTm                       { $1                                        } 

AppTm   :   ATm                         { $1                                        } 
        |   AppTm ATm                   { \c -> TmApp ($1 c)($2 c)                  } 
        |   succ ATm                    { \c -> TmSucc ($2 c)                       } 
        |   pred ATm                    { \c -> TmPred ($2 c)                       } 

ATm     :   '(' Tm ')'                  { $2                                        }
        |   zero                        { \c -> TmZero                              } 
        |   true                        { \c -> TmTrue                              } 
        |   false                       { \c -> TmFalse                             }
        |   x                           { \c -> TmVar $ getVar c $1                 } 
        
Ty      :   type                        { \c -> Type $1                             }
        |   set                         { \c -> Set                                 }
        |   prop                        { \c -> Prop                                }
        |   nat                         { \c -> Nat                                 }
        |   bool                        { \c -> Bool                                }
        |   truth                       { \c -> PTrue                               } 
        |   falsity                     { \c -> PFalse                              } 
    

{
parseError _ = error "!Parse Error" 


type Var = String 

getVar              :: [(Var,Bind)] -> Var -> Int 
getVar [] str       = error $ "cannot find " ++ str ++ " in context"
getVar (x:xs) str   = if fst x == str  then 0 else 1 + (getVar xs str)

addVar ctx str      = (str,NameBind):ctx 

}




