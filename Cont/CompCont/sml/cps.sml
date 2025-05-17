signature CPS = sig 

eqtype var 

datatype value      = VAR of var
                    | LABEL of var 
                    | INT of int 
                    | FLOAT of string 
                    | StRING of string 
    
datatype accesspath = OFFp of int
                    | SELp of int * accesspath ;;
    
    
datatype primop = + | Sub  | * | Div | Neg
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
    
    
datatype cexp   = RECORD of (value * accesspath) list * var * cexp 
                | SELECT of int * value * var * cexp
                | OFFSET of int * value * var * cexp
                | APP    of value * value list
                | FIX    of (var * var list * cexp) list * cexp 
                | SWITCH of value * cexp list
                | PRIMOP of primop * value list * var list * cexp list 
    
end 
