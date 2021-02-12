{ 
module Lexer where 
    
import Prelude hiding (EQ, LT, GT ) 
import System.IO (isEOF) 
import Syntax
}

%wrapper "basic"

$digit      = 0-9

$lowerCapital   = [a-z]
$upperCapital   = [A-Z] 
$alpha          = [a-zA-Z] 
@str            = $alpha    [$alpha $digit \_ \' ]*  
@ucid           = $upperCapital  [$alpha $digit \_ \' ]* 
@lcid           = $lowerCapital  [$alpha $digit \_ \' ]*



tokens :-
    $white+         ;
    "//".*          ;   
    "forall"        { \s -> ALL                      }
    "∀"             { \s -> ALL                      }
    "exists"        { \s -> EXISTS                   }
    "∃"             { \s -> EXISTS                   }
    "Set"           { \s -> SET                      }
    "Prop"          { \s -> PROP                     }
    "Nat"           { \s -> NAT                      }
    "Bool"          { \s -> BOOL                     }
    "Type" $digit+  { \s -> TYPE (read s)            }
    "Set"           { \s -> SET                      }
    \\              { \s -> LAMBDA                   }
    "if"            { \s -> IF                       }
    "then"          { \s -> THEN                     }
    "else"          { \s -> ELSE                     }
    "("             { \s -> LPAREN                   }
    ")"             { \s -> RPAREN                   }
    "S"             { \s -> SUCC                     }
    "O"             { \s -> ZERO                     }
    "."             { \s -> DOT                      }
    ":"             { \s -> COLON                    } 
    ";"             { \s -> SEMI                     }
    "iszero"        { \s -> ISZERO                   }
    "pred"          { \s -> PRED                     } 
    "succ"          { \s -> SUCC                     }
    "fun"           { \s -> LAMBDA                   }
    "true"          { \s -> TRUE                     } 
    "false"         { \s -> FALSE                    } 
    "True"          { \s -> TRUTH                    }
    "False"         { \s -> FALSITY                  }
    $digit+         { \s -> INT (read s)             } 
    @ucid           { \s -> UCID s                   } 
    @lcid           { \s -> LCID s                   } 


{

data Token  = LAMBDA
            | IF | THEN | ELSE 
            | TRUE | FALSE 
            | LPAREN | RPAREN 
            | COLON | SEMI | DOT 
            | PRED | SUCC | ZERO | ISZERO 
            | INT Int | UCID String | LCID String 
            | TYPE Int | SET | PROP                    
            | TRUTH | FALSITY 
            | NAT | BOOL                               
            | ALL | EXISTS                             
            deriving (Eq, Show)                        
                                                       
lex = alexScanTokens                                   
}                                                      
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
