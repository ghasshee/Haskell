{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE TypeFamilies #-} 

module Category.NaturalTransformation 

where

import Category.Category 
import Category.Functor


-- infixl 9 ! 


type f :~> g = forall c d. (c ~ Dom f, c ~ Dom g, d ~ Cod f, d ~ Cod g) => Nat c d f g

data Nat :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> * where
    Nat :: (Functor f, Functor g, c~Dom f, c~Dom g, d~Cod f, d~Cod g) 
        => f -> g -> (forall z. Obj c z -> Component f g z) -> Nat c d f g

type Component f g z = Cod f (f :% z) (g :% z)


(!) :: (Category c, Category d) => Nat c d f g -> c a b -> d (f:%a)(g:%b) 
Nat f _ n ! h = n (cod h) . f % h
