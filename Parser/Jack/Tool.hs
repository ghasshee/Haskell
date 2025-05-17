module Tool where

(...)           = (.)(.)(.)
(.....)         = (.)(.)(.)(.)(.) 

x               = flip

(|||)           :: [a->Bool] -> a -> Bool 
(|||)           = x(x(((...)foldr x)((.)(||)))False)

(&&&)           :: [a->Bool] -> a -> Bool 
(&&&)           = x(x(((...)foldr x)((.)(&&)))True) 

filtersOR       :: [a -> Bool] -> [a] -> [a] 
filtersOR       = x((...)concatMap x filter)   

filtersAND      :: [a -> Bool] -> [a] -> [a]
filtersAND      = undefined 

isMod           = (...)(==0)(flip mod) 
devides         = isMod 




{--
(|||)           :: (a->Bool) -> a -> Bool 
(|||) [] a      =  False
(|||) (f:fs) a  =  f a || (|||) fs a 
--} 




