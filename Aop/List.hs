module Aop.List where
import Aop.Int

data Listr a = Nil | Cons (a, Listr a)  
data Listl a = Lin | Snoc (Listl a, a)

instance (Show a) => Show(Listr a) where show l = "[" ++ showLr l ++ "]"
showLr Nil              = "[]"
showLr (Cons(a, Nil))   = show a
showLr (Cons(a, x))     = show a ++ "," ++ showLr x

instance (Show a) => Show(Listl a) where show l = "[" ++ showLl l ++ "]"
showLl Lin              = "[]"
showLl (Snoc(Lin,a))    = show a
showLl (Snoc(x,a))      = showLl x ++ "," ++ show a 

nil = Nil;   cons a x = Cons(a,x)
lin = Lin;   snoc a x = Snoc(x,a)

revl    = foldl' cons nil 
convl  Lin              = Nil
convl (Snoc(x,a))       = _snocr (convl x) a 
_snocr  Nil        a    = cons a nil
_snocr (Cons(a,x)) b    = cons a (_snocr x b)
snocr   = flip _snocr
revr    = foldr' snoc lin
convr  Nil              = Lin
convr (Cons(a,x))       = _consl a (convr x)
_consl a  Lin           = snoc a lin
_consl a (Snoc(x,b))    = snoc b (_consl a x)
consl   = _consl

foldr' h c Nil          = c 
foldr' h c (Cons (a,x)) = h a (foldr' h c x)  
foldl' h c Lin          = c
foldl' h c (Snoc(x,a))  = h a (foldl' h c x)
listr f = foldr' (cons . f) nil
listl f = foldl' (snoc . f) lin

sumr     = foldr' (+) 0
suml     = foldl' (+) 0
productr = foldr' (*) 1
productl = foldl' (*) 1
catr x y = foldr' cons y x 
catl x y = foldl' snoc y x 
concatr  = foldr' catr nil
concatl  = foldl' catl lin
lengthr  = sumr . listr one 
lengthl  = suml . listl one

classifier p f g a = if p a then f a else g a 
filterr p = concatr.listr(classifier p wrap nilp)
filterl p = concatl.listl(classifier p wlap linp)
wrap a = cons a nil
nilp a = nil
wlap a = snoc a lin
linp a = lin
filterrr p = foldr' (\a x->if p a then cons a x else x) nil
filterll p = foldl' (\a x->if p a then snoc a x else x) lin


fromListr = foldr' (:) []
fromListl = foldl' (:) []
toListr []      = nil
toListr (a:x)   = cons a (toListr x)
toListl []      = lin
toListl (a:x)   = consl a (toListl x)

taker n x = foldr' h c x n
    where   c n = nil
            h a f = fa
                where   fa 0 = nil
                        fa n = cons a (f (n-1))
dropr n x = foldr' h c x n 
    where   c n = nil
            h a f = fa
                where   fa 0 = cons a (f 0)
                        fa n = f (n-1)



--  foldr' h c [1,2,3,4] = h 1(h 2(h 3(h 4 c)))
--  foldl' h c [1,2,3,4] = h(h(h(h c 4)3)2)1
-- e.g.
ll = snoc 1 (snoc 2 (snoc 3 (snoc 4 (snoc 5 (snoc 7 lin)))))
lr = cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 7 nil)))))


