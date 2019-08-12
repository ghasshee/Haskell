module Aop.Inverse where 


import Aop.Int
import Aop.List
import Aop.Tree



zipr Nil _ = Nil
zipr _ Nil = Nil
zipr(Cons(a,x))(Cons(b,y)) = Cons((a,b),zipr x y)

unzipr = pair (listr outl, listr outr)
pair (f,g) x = (f x, g x)

unzipr' = foldr' conss nils
nils              = (nil,nil)
conss (a,b) (x,y) = (cons a x,cons b y)


evaln = foldl' f 0
f d n = 10*n + d



zipr' = foldr' ccons nnil

nnil    y = nil
ccons a f = fa
    where fa Nil = nil
          fa (Cons(b,y)) = Cons((a,b),(f y)) 


wide (Node(a,Lin))  = snoc a lin
wide (Node(a,x))    = snoc a (_wide x)
_wide x = catl (top x) (rest x)
top Lin = Lin
top (Snoc(y,Node(a,x))) = snoc a (top y)
rest Lin = Lin
rest (Snoc(y,Node(a,x))) = catl (_wide x) (rest y)

deep (Node(a,Lin))  = snoc a lin
deep (Node(a,x))    = snoc a (_deep x)
_deep Lin = Lin
_deep (Snoc(y,b))   = catl (deep b) (_deep y)  
