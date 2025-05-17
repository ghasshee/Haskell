module Math.Magma where

class Magma a where 
    append :: a -> a -> a 


-- Free Magma is just Binary Tree 
data FreeMagma a = Leaf a | Node (FreeMagma a) (FreeMagma a) deriving (Show)


data Term  = T Term Term | S String    
instance Show Term where
    show (S x)      = x
    show (T x y)    = "("  ++ show x ++ " " ++ show y ++ ")" 
instance Magma Term where
    append a b       = T a b 

--eg 
x = S"x"
y = S"y"
z = S"z"
xy = append x y 
yx = append y x
xy_x = append xy x
x_yx = append x yx

b x y z = append x (append y z)
m x = append x x 
s x y z = append (append x y) (append x z)
k x y = x
