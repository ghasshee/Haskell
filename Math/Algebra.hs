module Math.Algebra where 

import Aop.Tree



class Num a => Ring a 


data Module r a = Bree (r,a) 


data Algebra a = Algebra {
        x :: a -> Algebra a, 
        v :: a ,
        y :: a -> Algebra a
        } 

instance Show a => Show (a -> Algebra a) where 
    show x = "x"
    show y = "y" 


 -- instance Num a => Num Algebra a 
