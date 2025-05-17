
-- Monad 

{-- 

obj1    >>= \x ->
fun1 x  >>= \y ->
obj2    >>= \_ ->
fun2 y  >>= \z ->
return z

is equivalent to 

do{ x   <-  obj1    ;
    y   <-  fun1 x  ;
            obj2    ;
    z   <-  fun2 y  ;
    return z        }
--} 


-- Backus-Naur Form 
-- number   = [ "-" ] digit { digit }.
-- digit    = "0" | "1" | ... | "8" | "9".
-- expr     = term      { addop term    }.
-- term     = factor    { mulop factor  }.
-- factor   = "(" expr ")" | number.
-- addop    = "+" | "-"
-- mulop    = "*"


-- chainl              ::  Parser a -> Parser (a -> a -> a) -> a -> Parser a
-- chainl pa pop a     =   (pa `chainl1` pop) <|> return a

