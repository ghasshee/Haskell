use "cps.sml";



functor CPSsemantics(
    structure CPS:CPS
        val minint : int 
        val maxint : int 
        val minreal : real
        val maxreal : real
        val string2real : string -> real
        eqtype loc 
        val nextloc : loc -> loc 
        val arbitrarily : 'a * 'a -> 'a 
        type answer

        datatype dvalue = RECORD    of dvalue list * int
                    | INT       of int
                    | REAL      of real
                    | FUNC      of (dvalue list -> loc * (loc->dvalue) * (loc->int) -> answer) 
                    | STRING    of string 
                    | BYTEARRAY of loc list
                    | ARRAY     of loc list
                    | UARRAY    of loc list

        val handler_ref : loc 
        val overflow_exn : dvalue 
        val div_exn : dvalue 
    ) : sig 
        (*val eval : var list * cexp -> CPS.dvalue list -> (CPS.loc * (CPS.loc->CPS.dvalue) * (CPS.loc->int)) -> CPS.answer *)
    end = struct
        type store = loc * (loc -> dvalue) * (loc -> int) 
        fun fetch ((_,f,_): store) (l:loc) = f l 
        fun upd ((n,f,g):store) (l:loc) (v:dvalue) = (n, fn i => if i=l then v else f i, g)  




    end 




