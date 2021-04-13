module Type.STL_ext where

import Comonad.Comonad 

data Tree a' = Nd a' [Tree a']

instance Show a' => Show (Tree a') where 
    show t = showNd "   " t 
        where 
        showNd s (Nd a x)       = "+- " ++ show a ++ "\n" ++ showNds s x
        showNds s []            = ""
        showNds s [Nd a x]      = s ++ "+- " ++ show a ++ "\n" ++
                                  showNds (s++"   ") x ++ showNds s [] 
        showNds s ((Nd a x):xs) = s ++ "+- " ++ show a ++ "\n" ++
                                  showNds (s++"|  ") x ++ showNds s xs

foldt  g h d c (Nd a [])   = g a d 
foldt  g h d c (Nd a xs)   = g a (foldts g h d c xs) 
foldts g h d c []          = c 
foldts g h d c (x:xs)      = h (foldt g h d c x) (foldts g h d c xs) 

data AST a' = Tm a' [AST a'] 
                deriving Show 

tree2term = foldt Tm (:) [] []

instance Functor Tree where     
    fmap f (Nd a [])            = Nd (f a) []
    fmap f (Nd a (x:xs))        = Nd (f a) (fmap f x: fmap (fmap f) xs)

instance Comonad Tree where 
    extract (Nd a _)            = a 
    extend fold (Nd b [])       = Nd (fold (Nd b [])) []
    extend fold (Nd b (x:xs))   = Nd (fold (Nd b(x:xs))) (fmap(extend fold)(x:xs))


-- Try: 
-- >>> extend sum_tree tt 


-----------------------
-- Simple Calculator --
-----------------------

--------------------------------
-- Simple Type LambdaCalculus --
--------------------------------

type Ty     = Term 

data Term   = Var Int 
            | Abs String Ty
            | App 
            | Record
            | Univ Int
            | Pi String Ty 
            | Sigma String Ty 
            | Bool 
            | Nat 
            | S 
            | O
            | T
            | F
            deriving Show 

tmVar i         = Tm (Var i) []
tmAbs x tyT t   = Tm (Abs x tyT) [t] 
tmApp t t'      = Tm App [t, t'] 
tmRecord ts     = Tm Record ts 
tmUniv i        = Tm (Univ i) []
tmPi x tyT t    = Tm (Pi x tyT) [t]
tmSigma x tyT t = Tm (Sigma x tyT) [t] 
tmBool          = Tm (Bool) []
tmNat           = Tm (Nat) []
tmS t           = Tm (S) [t] 
tmO             = Tm (O) []
tmT             = Tm (T) []
tmF             = Tm (F) []


ndVar i         = Nd (Var i) []
ndAbs x tyT t   = Nd (Abs x tyT) [t] 
ndApp t t'      = Nd App [t, t'] 
ndRecord ts     = Nd Record ts
ndUniv i        = Nd (Univ i) []
ndPi x tyT t    = Nd (Pi x tyT) [t]
ndSigma x tyT t = Nd (Sigma x tyT) [t] 
ndBool          = Nd Bool []
ndNat           = Nd Nat []
ndS t           = Nd S [t]
ndO             = Nd O []
ndT             = Nd T []
ndF             = Nd F []


eval (Nd (Var i) [])                    = tmVar i 
eval (Nd (Abs x tyT) [t])               = tmAbs x tyT (eval t)
eval (Nd App [Nd(Abs x tyT)[t], t2])    = tree2term (tmSubstTop t2 t) 
eval (Nd Record ts)                     = tmRecord $ map eval ts   
eval (Nd (Univ i) [])                   = tmUniv i 
eval (Nd (Pi x tyT) [t])                = tmPi x tyT (eval t) 
eval (Nd (Sigma x tyT) [t])             = tmSigma x tyT (eval t) 
eval (Nd Bool [])                       = tmBool
eval (Nd Nat [])                        = tmNat
eval (Nd S [t])                         = tmS (eval t) 
eval (Nd O [])                          = tmO 
eval (Nd T [])                          = tmT
eval (Nd F [])                          = tmF

walk onVar c t = case t of 
    Nd (Var k) []       -> onVar c k
    Nd (Abs x tyT) [t]  -> Nd (Abs x tyT) [walk onVar (c+1) t]
    Nd App [t1,t2]      -> Nd App [walk onVar c t1,walk onVar c t2]
    Nd Record ts        -> Nd Record $ fmap (walk onVar c) ts
    Nd (Univ i)[]       -> Nd (Univ i) [] 
    Nd (Pi x tyT) [t]   -> Nd (Abs x tyT) [walk onVar (c+1) t] 
    Nd (Sigma x tyT)[t] -> Nd (Sigma x tyT) [walk onVar (c+1) t] 
    Nd Bool []          -> Nd Bool []
    Nd Nat []           -> Nd Nat []
    Nd S [t]            -> Nd S [walk onVar c t] 
    Nd O []             -> ndO
    Nd T []             -> ndT
    Nd F []             -> ndF


tmShiftOnVar d c k      = if k>=c then ndVar (k+d) else ndVar k
tmShift d               = walk (tmShiftOnVar d) 0 

tmSubstOnVar j s t c k  = if k==j+c then tmShift c s else ndVar k
tmSubst      j s t      = walk (tmSubstOnVar j s t) 0 t
tmSubstTop     s t      = tmShift(-1)(tmSubst 0 (tmShift 1 s) t)


-- e.g.
ty = Bool 
test = ndRecord [ndApp (ndAbs "x" ty (ndVar 0)) (ndVar 4), ndVar 5, ndAbs "y" ty (ndVar 1)] 

