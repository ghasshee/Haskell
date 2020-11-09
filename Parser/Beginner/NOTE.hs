
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

