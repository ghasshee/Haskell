
open Cps 
(* #use "CPS.ml";; *) 


module CPSsemantics (C: 
    sig
        val minint : int 
        val maxint : int 
        val minfloat : float
        val maxfloat : float
        val string2float : string -> float
        type loc 
        val nextloc : loc -> loc 
        val arbitrarily : 'a * 'a -> 'a 
        type answer

        type dvalue = RECORD    of dvalue list * int
                    | INT       of int
                    | FLOAT     of float
                    | FUNC      of (dvalue list -> loc * (loc->dvalue) * (loc->int) -> answer) 
                    | STRING    of string 
                    | BYTEARRAY of loc list
                    | ARRAY     of loc list
                    | UARRAY    of loc list

        val handler_ref : loc 
        val overflow_exn : dvalue 
        val div_exn : dvalue 
    end) : sig 
        (*val eval : var list * cexp -> CPS.dvalue list -> (CPS.loc * (CPS.loc->CPS.dvalue) * (CPS.loc->int)) -> CPS.answer *)
    end = struct
        type store = C.loc * (C.loc -> C.dvalue) * (C.loc -> int) 
        let fetch ((_,f,_): store) (l:C.loc) = f l 
        let upd ((n,f,g):store) (l:C.loc) (v:C.dvalue) = (n,(fun i -> if i=l then v else f i), g)  




    end 


let _ = print_string "hoge" 


