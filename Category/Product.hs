{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module Category.Product where 

import Category.Category



data (:**:) :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> * where
     (:**:) :: c1 a1 b1 -> c2 a2 b2 -> (:**:) c1 c2 (a1,a2) (b1,b2)



instance (Category c1, Category c2) => Category (c1 :**: c2) where 
    dom (x1 :**: x2)                = dom x1    :**: dom x2 
    cod (x1 :**: x2)                = cod x1    :**: cod x2 
    (g1 :**: g2) . (f1 :**: f2)     = (g1 . f1) :**: (g2 . f2) 




