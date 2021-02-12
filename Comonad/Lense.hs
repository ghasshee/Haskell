module Lense where

import Control.Lens


-- Person Data Type  
data Person             = Person { _firstName :: String, _lastName :: String }  
        deriving Show

makeLenses ''Person     -- Automate the following definions 

{-- 
type Simple_Lens a b = Lens a a b b 

getFirst, getLast       ::  Person -> String 
getFirst (Person f _)   =   f
getLast  (Person _ l)   =   l
setFirst, setLast       ::  Person -> String -> Person 
setFirst (Person _ l) f =   Person f l 
setLast  (Person f _) l =   Person f l 

firstName, lastName     :: Simple_Lens Person String
firstName               = lens getFirst setFirst
lastName                = lens getLast setLast
--} 


-- e.g. 
p1 = Person "Nihon" "Taro" 
p2 = Person "Osaka" "Hanako" 
change_p1 = over firstName (++ "maru") p1 
change_p2 = over firstName (const "Kyoto") p2 
-- getter == view 
-- setter == over 
-- ghci> over firstName (\p -> p ++ "maru") p1 
-- ghci> view firstName p2 




----------- Client Data Type ----------------
data Client i           = GovOrg      i String 
                        | Company     i String Person String
                        | Individual  i Person

getID client        = case client of  
    GovOrg i _          -> i
    Company i _ _ _     -> i
    Individual i _      -> i
setID client i      = case client of 
    GovOrg _ n          -> GovOrg i n
    Company _ n p r     -> Company i n p r
    Individual _ p      -> Individual i p 

identifier :: Lens (Client i)(Client j) i j
identifier = lens getID setID


-- e.g.
c1 = GovOrg     1 "Japan" 
c2 = GovOrg     2 "US" 
c3 = Company    3 "Amazon" (Person "Jeff" "Bezov") "President" 



{--
get :: Lens s s a a -> s -> a 
get f s = let Store _ a = f (\a -> Store undefined a) s in a 

set :: Lens s t a b -> s -> (b->t) 
set f s = \b -> let Store v r = f(Store(const b)) s in v r 
--} 


{-# LANGUAGE TemplateHaskell #-}

person2string :: Person -> String
person2string (Person f l) = f ++ " " ++ l

setFullName :: Person -> String -> Person
setFullName _ fullname = case words fullname of
    f:l:_   -> Person f l
    _       -> error "Incorrect name"


fullName :: Simple_Lens Person String
fullName = lens person2string setFullName



makeLenses ''Client     --- ??? 



