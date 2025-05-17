{-# LANGUAGE RankNTypes #-}
newtype Coyoneda f x = Coyoneda (forall a.(x->a) -> f a) 

liftCoyoneda x f = fmap f x
pureCoyoneda (Coyoneda y) = y id 

