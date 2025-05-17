

module Type where

import Syntax
import Lexer
import Parser
import Eval










typeof ctx t    = case t of 
    Univ k          ->  Univ (k+1) 
    Lam x t e       ->  let Univ k = typeof ctx t in  -- Check the type of t is Univ
                        Pi x t (typeof (extend x t ctx) e) 
    Pi x t1 t2      ->  let Univ k1 = eval ctx (typeof ctx t1) in 
                        let Univ k2 = eval ctx (typeof ctx t2) in 
                        Univ (max k1 k2) 
    App e1 e2       ->  let Pi x t11 t12 = eval ctx (typeof ctx e1) in 
                        let t2 = typeof ctx e2 in 
                        if equal ctx t11 t2 then subst e2 t12 
    

