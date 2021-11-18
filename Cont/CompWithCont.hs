module CompWithCont where

type Var    = Int 

data Value  = VAR Var
            | LABEL Var
            | INT Int
            | REAL String
            | STRING String 
            deriving (Show, Eq)

data AccessPath = OFFp Int
                | SELp Int AccessPath
                deriving (Show, Eq) 



data PrimOp = Add | Sub  | Mul | Div | Neg
            | Eq  | Neq  | Lt  | Le  | Gt | Ge 
            | RngChk 
            | Bang | Subscript | OrdOf
            | Assign | UnboxedAssign | Update | UnboxedUpdate | Store 
            | MkRef | MkRefUnboxed | ALength | SLength 
            | GetHdlr | SetHdlr
            | Boxed 
            | FAdd | FSub | FMul | FDiv 
            | FEq  | FNeq | FLt  | FLe | FGt | FGe 
            | RShift | LShift | OrB | AndB | XorB | NotB
            deriving (Show, Eq) 

data CExp   = RECORD [(Value,AccessPath)] Var CExp 
            | SELECT Int Value Var CExp
            | OFFSET Int Value Var CExp
            | APP Value [Value] 
            | FIX [(Var, [Var], CExp)] CExp
            | SWITCH Value [CExp] 
            | PRIMOP PrimOp [Value] [Var] [CExp] 
            deriving (Show, Eq) 



t1 = PRIMOP Add [VAR 1, VAR 2] [3] [] 
if_a_gt_b_then_f_else_g = PRIMOP Gt [VAR 1,VAR 2] [] [f,g] 
    where (f,g) = undefined 






