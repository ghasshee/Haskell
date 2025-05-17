
--{-# LANGUAGE RankNTypes         #-} 
{-# LANGUAGE NoImplicitPrelude      #-} 
{-# LANGUAGE TypeOperators          #-} 
{-# LANGUAGE TypeFamilies           #-} 
{-# LANGUAGE GADTs                  #-} 
{-# LANGUAGE UndecidableInstances   #-} 



module Category.Functor where

import Category.Category

infixr 9 %
infixr 9 :%



{--  f      :: Category c -> Category d 
 -  Dom f   := c 
 -  Cod f   := d 
 -  f :% x  := f(x) 
 -  (%)     := f
 --} 

class (Category (Dom _F), Category (Cod _F)) => Functor _F where 
    type Dom _F     :: * -> * -> *
    type Cod _F     :: * -> * -> *
    type _F :% x    :: *           
    (%)             :: _F -> (Dom _F) x y -> (Cod _F)(_F:%x)(_F:%y)


data Cat    :: * -> * -> * where 
     CAT    :: (Functor _F, Category(Dom _F),Category(Cod _F)) => 
                _F  ->  Cat (OB(Dom _F))(OB(Cod _F))
    -- CAT  :: FUNCTOR F |-> MORPHISM F in CAT
    --
    -- CAT    :=  F   ->  Cat (Ob_CAT c) (OB_CAT d)  



-- | We need a wrapper becuase objects need to be of kind *, 
-- | and categories are of kind * -> * -> *
data OB :: (* -> * -> *) -> *


instance Category Cat where 
    dom (CAT _)         = CAT Id 
    cod (CAT _)         = CAT Id
    CAT _G . CAT _F     = CAT (_G :.: _F)


data Id (k :: * -> * -> *) = Id 


instance Category k => Functor (Id k) where 
    type Dom (Id k)     = k
    type Cod (Id k)     = k 
    type Id k :% a      = a 
    _ % f               = f 


data (h :.: g) where 
    (:.:) :: (Functor h, Functor g, Dom h ~ Cod g) => h -> g -> h :.: g 

instance (Category (Cod h), Category(Dom g)) => Functor (h :.: g) where 
    type Dom (h :.: g) = Dom g
    type Cod (h :.: g) = Cod h
    type (h :.: g) :% a = h :% (g :% a) 
    (h:.:g) % f = h % (g % f) 



data Const (c1 :: * -> * -> *) (c2 :: * -> * -> *) x where 
    Const :: Category c2 => Ob c2 x -> Const c1 c2 x


data Opposite f where 
    Opposite :: Functor f => f -> Opposite f 

instance (Category(Dom f), Category(Cod f)) => Functor (Opposite f) where 
    type Dom(Opposite f)    = Op (Dom f)
    type Cod(Opposite f)    = Op (Cod f) 
    type Opposite f :%a     = f :% a 
    Opposite f % Op a       = Op (f % a)



