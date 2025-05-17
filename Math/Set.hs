module Math.Set where 


data Color  = R | B deriving (Show,Eq)
data Tree a = E | T Color (Tree a) a (Tree a) deriving Eq 
instance (Show a,Eq a)=> Show (Tree a) where
    show E              = ""
    show (T _ a x b)    = replicate(d-1)' ' ++ show x ++ "\n" ++ horiz d [a,b] where
        d                   = 32
        horiz d []          = ""
        horiz d l           =
            replicate ((div d 2)-1) ' ' ++ _top d l ++ "\n" ++ horiz (div d 2) (_rest l) where
            _top d []                   = ""
            _top d l | _all_E l         = ""
            _top d (E:xs)               = replicate d ' ' ++ _top d xs
            _top d ((T _ _ x _):xs)     = show x ++ replicate (d-1) ' ' ++ _top d xs
            _rest []                    = []
            _rest l | _all_E l          = []
            _rest (E:xs)                = [E,E] ++ _rest xs
            _rest((T _ a _ b):xs)       = [a,b] ++ _rest xs
            _all_E  []                  = True
            _all_E  (x:xs)              = if x /= E then False else _all_E xs



type Set a = Tree a

empty = E
member :: Ord a => a -> Set a -> Bool
member x E = False
member x (T _ a y b)    | x < y     = member x a
                        | x == y    = True
                        | x > y     = member x b


insert :: Ord a => a -> Set a -> Set a 
insert x s = makeBlack (ins s) where
    ins E                   = T R E x E
    ins (T c a y b) | x<y   = balance c (ins a) y b
                    | x==y  = T c a y b
                    | x>y   = balance c a y (ins b)
    makeBlack (T _ a y b)   = T B a y b

balance B (T R (T R a x b) y c) z d     = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d))     = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d)     = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d     = T R (T B a x b) y (T B c z d)
balance c a x b                         = T c a x b

traverseT E             = [] 
traverseT (T _ x a y)   = a : horizontal [x,y] where
    horizontal []           = []
    horizontal l            = top l ++ horizontal (rest l) where
        top []                  = []
        top (E:xs)              = top xs
        top ((T _ _ x _):xs)    = x: top xs
        rest []                 = []
        rest (E:xs)             = rest xs
        rest ((T _ a _ b):xs)   = [a,b] ++ rest xs  

i = insert

t = i 11 $ i 12 $ i 13 $ i 14 $ i 1 $ i 2 $ i 3 $ i 8 $ i 10 $ i 4 $ i 7 $ i 6 $ i 9 $ i 5 E
s = i 15 t
u = i 16 s
