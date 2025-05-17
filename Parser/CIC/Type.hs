

module Type where

import Syntax
import Lexer
import Parser




typeof ctx t = case t of 
    Type i      -> Type (i+1) 
    Set         -> Type 0 
    Prop        -> Type 0 
    Nat         -> Set
    Bool        -> Set
    PTrue       -> Prop
    PFalse      -> Prop

    

