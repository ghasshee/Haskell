{-# LANGUAGE FunctionalDependencies #-} 
{-# LANGUAGE MultiParamTypeClasses  #-} 
{-# LANGUAGE TypeOperators #-}

module State where 

data Transition s a = Transition (s->(s,a)) 
runTransition transition = let Transition f = transition in f 

instance Functor (Transition s) where 
    fmap g (Transition f)   = Transition $ \s -> let (s',a) = f s in (s',g a) 

instance Applicative (Transition s) where 
    pure a                      = Transition $ \s -> (s,a) 
    Transition f<*>Transition a = Transition $ \s -> let (s',f') = f s in let (s'',a') = a s' in (s'', f' a') 

instance Monad (Transition s) where 
    Transition f >>= m         = Transition $ \s -> let (s',a) = f s in runTransition (m a) s'  




class (Functor f, Functor g) => Adjunction f g | f -> g, g -> f where 
    unit    ::  a -> g (f a) 
    counit  ::  f (g a) -> a 

    leftAdj ::  (f a -> b) -> (a -> g b) 
    rightAdj::  (a -> g b) -> (f a -> b) 

    unit    = leftAdj id 
    counit  = rightAdj id 



-- newtype (.) g f a = Compose {getCompose :: g(f a) }
data (.) g f a = Compose (g (f a)) 
getCompose gfa = let Compose gfa' = gfa in gfa' 

instance (Functor f, Functor g) => Functor (g . f) where 
    fmap f (Compose gf) =  Compose $ fmap (fmap f) gf


join :: Adjunction f g => g (f (g (f a))) -> g (f a)
join = fmap counit 

join' :: Adjunction f g => (g.f) ((g.f) a) -> (g . f) a
join' (Compose gfgf) = Compose . join $ fmap (fmap getCompose) gfgf
