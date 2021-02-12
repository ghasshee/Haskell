{-# LANGUAGE NoImplicitPrelude #-} 

module Category.Presheaf where 

import Category.Category 
import Category.Functor 
import Category.NaturalTransformation



type Presheaves k = Nat (Op k) (->) 


-- type PShExponential k y z = (Presheafves k :-*: z) :.: Opposite 

