{ 
    module Main where 
    
}

%wrapper "basic"

$digit  = 0-9
$alpha  = [a-zA-Z] 
@str    = $alpha [$alpha $digit \_ \' ]*  

tokens :-
    $white+     ;
    "//".*      ;   
    "push"      { \s -> PUSH                                                    }
    "pop"       { \s -> POP                                                     }
    "argument"  { \s -> ARGUMENT                                                }
    "local"     { \s -> LOCAL                                                   }
    "static"    { \s -> STATIC                                                  }
    "constant"  { \s -> CONST                                                   }
    "this"      { \s -> THIS                                                    }
    "that"      { \s -> THAT                                                    }
    "pointer"   { \s -> POINTER                                                 }
    "temp"      { \s -> TEMP                                                    } 
    "add"       { \s -> ADD                                                     }
    "sub"       { \s -> SUB                                                     }
    "neq"       { \s -> NEQ                                                     }
    "eq"        { \s -> Eq                                                      }
    "gt"        { \s -> Gt                                                      }
    "lt"        { \s -> Lt                                                      }
    "and"       { \s -> AND                                                     } 
    "or"        { \s -> OR                                                      }
    "not"       { \s -> NOT                                                     }
    "label"     { \s -> LABEL                                                   }
    "goto"      { \s -> GOTO                                                    }  
    "if-goto"   { \s -> IFGOTO                                                  }
    "function"  { \s -> FUN                                                     }
    "call"      { \s -> CALL                                                    }
    "return"    { \s -> RETURN                                                  }
    "∀"         { \s -> FORALL                                                  } 
    $digit+     { \s -> INT (read s)                                            } 
    @str        { \s -> STRING s                                                } 


{
data Token  = PUSH | POP | FORALL
            | ARGUMENT | LOCAL | STATIC | CONST | THIS | THAT | POINTER | TEMP
            | ADD | SUB | NEQ | Eq | Gt | Lt | AND | OR | NOT 
            | LABEL | GOTO | IFGOTO | FUN | CALL | RETURN 
            | INT Int | STRING String  deriving (Eq, Show) 

loop = do 
    s <- getLine
    print $ alexScanTokens s
    loop 

main = loop 

} 
