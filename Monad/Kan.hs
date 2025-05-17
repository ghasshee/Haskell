{-# LANGUAGE GADTs #-} 

module Kan where

newtype Ran k d a = Ran (forall i . ( a -> k i ) -> d i ) 


type Lst a = forall i . Monoid i => (a -> i) -> i 

toLst :: [a] -> Lst a 
toLst l = \f -> foldMap f l 

fromLst :: Lst a -> [a] 
fromLst f = f (\a -> [a]) 


data Lan k d a = forall i . Lan (k i -> a) (d i) 




