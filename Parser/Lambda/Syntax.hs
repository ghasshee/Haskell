{-# LANGUAGE DataKinds #-} 


module Syntax where 


---- Type & Term ----




data Term       = Var Int 
                | Univ Int 
                | Pi  String Term Term 
                | Lam String Term Term 
                | App Term Term 
                deriving (Show, Eq) 




---- Context ----

type Context    = [(String, Bind)] 


---- Bind ----

data Bind       = NameBind
                | VarBind  
                | TyVarBind Term


---- Goal ----

data Goal       = Goal Context Term



