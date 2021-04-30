module Cont.Sat where 


import Data.Map



data BF = Var String 
        | Not BF 
        | And BF BF 
        | Or  BF BF

-- SAT solver can be implemented with CPS 
-- fail is continuation 
sat :: BF -> Map String Bool -> (Bool -> Map String Bool -> k -> k) -> k -> k
sat bf asst succ fail = case bf of 
    Var v   -> case asst !? v of 
        Just b  -> succ b asst fail 
        Nothing -> let  asstT = insert v True asst 
                        asstF = insert v False asst 
                        tryT  = succ True asstT tryF
                        tryF  = succ False asstT fail in tryT 
    Not bf' -> sat bf' asst (succ . not) fail 
    And l r -> sat l asst succAnd fail where 
                    succAnd True asstAnd failAnd    = sat r asstAnd succ failAnd
                    succAnd False asstAnd failAnd   = succ False asstAnd failAnd
    Or  l r -> sat l asst succOr fail where
                    succOr True asstOr failOr       = succ True asstOr failOr 
                    succOr False asstOr failOr      = sat r asstOr succ failOr 


                

solve :: BF -> Maybe (Map String Bool) 
solve bf = sat bf empty (\b asst fail -> if b then Just asst else fail) Nothing 
