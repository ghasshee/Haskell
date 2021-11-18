{-# LANGUAGE TypeFamilies       #-} 
{-# LANGUAGE GADTs              #-} 
{-# LANGUAGE RankNTypes         #-} 
{-# LANGUAGE NoImplicitPrelude  #-} 
{-# LANGUAGE PolyKinds          #-} 



module Category.Category where 



infixr 8 . 

{-- OBJECT 
Ob_c(x)  :: Hom_c(x,x) 
Ob_c(x)  =  id_x 
--} 


type Ob c x = c x x  


class Category c where 
    dom :: c x y    -> Ob c x  
    cod :: c x y    -> Ob c y  
    (.) :: c y z    -> c x y    -> c x z 


{-- 
 - dom      ::  Hom_c(x,x)  ->  Ob_c(x) 
 - dom      =       _       ->  id_x      
 - cod      ::  Hom_c(x,x)  ->  Ob_c(y)
 - cod      =       _       ->  id_y  
 - (.)      ::  Hom_c(y,z)  ->  Hom_c(x,y)  ->  Hom_c(x,z) 
 - (.)      =       g       ->      f       ->   g . f     
--}


instance Category (->) where 
    dom _ = \x -> x 
    cod _ = \y -> y 
    f . g = \x -> f (g x)



{--
 -   c    y x := Hom_c(y,x)
 - (Op c) x y := Hom_opc(x,y) 
 -
 -     f        :: c y x
 -  Op f        :: c x y
--}


data Op c a b = Op { unOp :: c b a } 


instance Category c => Category (Op c) where 
    dom (Op f)      = Op (cod f) 
    cod (Op f)      = Op (dom f) 
    (Op f) . (Op g) = Op (g . f) 


type family Kind (k :: o -> o -> *) :: * where 
    Kind (k :: o -> o -> *) = o 


