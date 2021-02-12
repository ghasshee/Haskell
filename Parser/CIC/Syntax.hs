{-# LANGUAGE DataKinds #-} 


module Syntax where 



---- Type & Term ----
type Type       = Term 

data Term       = TmVar Int 
                | TmAbs Type Term
                | TmApp Term Term 
                | TmFalse 
                | TmTrue
                | TmIf Term Term Term 
                | TmZero 
                | TmSucc Term 
                | TmPred Term 
                | Type Int 
                | Prop 
                | Set 
                | PFalse | PTrue
                | Nat 
                | Bool 
                | All Term Term Term
                | Exists Term Term Term 
                deriving (Show,Eq) 




---- Context ----


type Context    = [(String, Bind)] 


---- Bind ----

data Bind       = NameBind
                | VarBind  
                | TyVarBind Term


---- Goal ----

data Goal       = Goal Context Term



