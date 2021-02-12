module Lense2 where 

import Control.Lens

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}


data Client i   = GovOrg    { _identifier :: i, _name :: String }
                | Company   { _identifier :: i, _name :: String, 
                              _person :: Person, _duty :: String }
                | Individual{ _identifier :: i, _person :: Person }
                deriving Show 

data Person     = Person { _firstName :: String, _lastName :: String } 
                deriving Show

-- This defines a lense for each argument
makeLenses ''Client
makeLenses ''Person

-- e.g. 
-- let p = Person "Vitalik" "Buterin" in (view firstName p, p^.lastName)
-- where 
--  view    :: MonadReader s m  => Getting a s a -> m a
--  firstName :: Functor f      => (String -> f String) -> Person -> f String
--  person :: Applicative f     => (Person -> f Person) -> Client i -> f (Client i)
-- (^.)     ::                     s -> Getting a s a -> a
-- type Getting r s a           =  (a -> Const r a) -> s -> Const r s 


showPerson :: Person -> String
showPerson (Person f l) = f ++ " " ++ l

setFullName :: Person -> String -> Person
setFullName _ fullname = case words fullname of
    f:l:_   -> Person f l
    _       -> error "Incorrect name"

fullName :: Simple Lens Person String
fullName = lens showPerson setFullName


