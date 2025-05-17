module Eval where 

import Syntax
import Lexer
import Parser



{------------------ Subst --------------------} 

walk onVar c t          =   case t of 
    App t1 t2               -> App (walk onVar c t1) (walk onVar c t2) 
    Lam x ty t              -> Lam x ty (walk onVar (c+1) t) 
    Pi  x ty t              -> Pi  x ty (walk onVar (c+1) t) 
    Var i                   -> onVar c i 
    Univ k                  -> Univ k 

shiftOnVar d          =   \c i -> if i>=c then Var (d+i) else Var i 
shiftAbove d          =   walk (shiftOnVar d) 
shift d               =   shiftAbove d 0 

substOnVar j t t2     =   \c i -> if i==j+c then shift c t else Var i 
subst  j t t2         =   walk (substOnVar j t t2) 0 t2
substTop t t2         =   shift (-1) (subst 0 (shift 1 t) t2)

{-----------------  Ctx  --------------------} 

extend x ty ctx     = (x,ty):ctx 

{-----------------   Eq   -------------------}

equal ctx e1 e2     = eq (eval ctx e1)(eval ctx e2)  where 
    eq v1 v2  = case (v1,v2) of  
        (Var i,Var j)           -> i == j 
        (App t1 t2,App t3 t4)   -> eq t1 t2 && eq t3 t4 
        (Univ k, Univ l)        -> k == l 
        (Pi x t e,Pi y s f)     -> eq t s && eq e f
        (Lam x t e,Pi y s f)    -> eq t s && eq e f 
        (_,_)                   -> False

{------------------ Eval --------------------} 



eval ctx t = case t of 
    Var i           ->  t 
    App e1 e2       ->  let v2 = eval ctx e2 in 
                        case eval ctx e1 of 
                            Lam x t e   -> eval ctx (substTop e2 e) 
                            Pi  x t e   -> eval ctx (substTop e2 e) 
                            e1          -> App e1 v2 
    Univ k          ->  t 
    Pi  x ty t      ->  Pi  x ty (eval (extend x ty ctx) t )
    Lam x ty t      ->  Lam x ty (eval (extend x ty ctx) t )





{------------------ CoEval  --------------------} 


coeval ctx t              =   case eval1 ctx t of 
    Nothing                 -> t 
    Just t'                 -> coeval ctx t' 
     

eval1                   ::  Context -> Term -> Maybe Term 
eval1 ctx t             =   case t of 
    App (Lam x ty t') t2    -> Just $ substTop t2 t' 
    App t1 t2               -> case eval1 ctx =<< Just t1 of 
        Nothing                 -> Nothing 
        Just t1'                -> Just $ App t1' t2 
    Lam x ty t'             -> Nothing -- return . Lam =<< eval1 ctx t'   
    Var i                   -> Nothing  




