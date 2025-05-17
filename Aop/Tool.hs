module Tool where

(...)           = (.)(.)(.)
(.....)         = (.)(.)(.)(.)(.) 

x               = flip

(|||)           :: [a->Bool] -> a -> Bool 
(|||)           = x(x(((...)foldr x)((.)(||)))True)

filtersOR       :: [a -> Bool] -> [a] -> [a] 
filtersOR       = x((...)concatMap x filter)   

filtersAND      :: [a -> Bool] -> [a] -> [a]
filtersAND      = undefined 

