module Music.KeyChord where

import Euterpea

toKey           :: Int -> Octave -> Dur -> Music Pitch
toKey 0         =  c
toKey 1         =  cs
toKey 2         =  d
toKey 3         =  ds 
toKey 4         =  e
toKey 5         =  f
toKey 6         =  fs
toKey 7         =  g
toKey 8         =  gs
toKey 9         =  a 
toKey 10        =  as
toKey 11        =  b

k n                         =   (toKey $ mod n 12) (div n 12) 
mute m n | n == m           = rest
         | otherwise        = k n 

kchord n x =  case mod x 12 of 
        y | y `elem` map (\m->mod(m+n)12) [1,3,6,8]  -> rest
          | otherwise                                -> k x 

kchord' 0 = k
kchord' 1 = kcM
kchord' 2 = kcsM
kchord' 3 = kdM
kchord' 4 = kdsM 
kchord' 5 = keM 
kchord' 6 = kfM 
kchord' 7 = kfsM
kchord' 8 = kgM 
kchord' 9 = kgM' 
kchord' _ = error  "chord' n : such n is not defined yet." 

kcM  x  | x `elem` [49,51,54,56]    = rest
        | otherwise                 = k x  
kcsM x  | x `elem` [50,52,55,57]    = rest
        | otherwise                 = k x  
kdM  x  | x `elem` [48,51,53,56]    = rest
        | otherwise                 = k x  
kdsM x  | x `elem` [49,52,54,57]    = rest
        | otherwise                 = k x  
keM  x  | x `elem` [48,50,53,55]    = rest
        | otherwise                 = k x  
kfM  x  | x `elem` [49,51,54,56]    = rest
        | otherwise                 = k x  
kfsM x  | x `elem` [48,50,52,55]    = rest
        | otherwise                 = k x  
kgM  x  | x `elem` [49,51,53,56]    = rest
        | otherwise                 = k x  
kgM' x  | x `elem` [48,49,51,53,56] = rest
        | otherwise                 = k x  


