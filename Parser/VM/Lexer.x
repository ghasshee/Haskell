{ 
module Lexer where 
    
import Prelude hiding (EQ, LT, GT) 
import System.IO (isEOF) 
}

%wrapper "basic"

$digit  = 0-9
$alpha  = [a-zA-Z] 
@str    = $alpha [$alpha $digit \_ \' \. ]*  
@space  = [\  \t \n]+

tokens :-
    $white+     ;
    "//".*      ;   
    @space      ; 
    "push"      { \s -> PUSH                                                    }
    "pop"       { \s -> POP                                                     }
    "argument"  { \s -> ARG                                                     }
    "local"     { \s -> LOCAL                                                   }
    "static"    { \s -> STATIC                                                  }
    "constant"  { \s -> CONST                                                   }
    "this"      { \s -> THIS                                                    }
    "that"      { \s -> THAT                                                    }
    "pointer"   { \s -> POINTER                                                 }
    "temp"      { \s -> TEMP                                                    } 
    "add"       { \s -> ADD                                                     }
    "sub"       { \s -> SUB                                                     }
    "neg"       { \s -> NEG                                                     }
    "eq"        { \s -> EQ                                                      }
    "gt"        { \s -> GT                                                      }
    "lt"        { \s -> LT                                                      }
    "and"       { \s -> AND                                                     } 
    "or"        { \s -> OR                                                      }
    "not"       { \s -> NOT                                                     }
    "label"     { \s -> LABEL                                                   }
    "goto"      { \s -> GOTO                                                    }  
    "if-goto"   { \s -> IFGOTO                                                  }
    "function"  { \s -> FUN                                                     }
    "call"      { \s -> CALL                                                    }
    "return"    { \s -> RET                                                     }
    $digit+     { \s -> INT (read s)                                            } 
    @str        { \s -> STRING s                                                } 


{
data Token  = PUSH | POP 
            | ARG | LOCAL | STATIC | CONST | THIS | THAT | POINTER | TEMP
            | ADD | SUB | NEG | EQ | GT | LT | AND | OR | NOT 
            | LABEL | GOTO | IFGOTO | FUN | CALL | RET 
            | INT Int | STRING String  deriving (Eq, Show) 


lex = alexScanTokens 

{--
main = do 
    done <- isEOF
    if done then return () else do 
        s <- getContents
        -- s <- getLine 
        print (alexScanTokens s) 
        --main 
--}
} 
