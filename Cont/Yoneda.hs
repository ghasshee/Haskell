{-# LANGUAGE UnicodeSyntax #-} 

module Yoneda where 

data I a = I a 

instance Functor I where 
    fmap f (I a) = I (f a) 


{--

instance Functor ((->) a) where 
    fmap f = (.) f 

--} 



-- #################################
-- ##     COYONEDA LEMMA          ## 
-- #################################

internalize :: Functor f => (∀ k. (a->k)->f k) -> f a 
internalize t = t id 
externalize :: Functor f => f a -> (∀ k. (a->k)->f k) 
externalize a k = fmap k a 




