module Music.Percussion where

import Euterpea
tom     = perc HighFloorTom 
hand    = perc HandClap

{-- Syncopation --} 
synco 1 per dur         = times dur $ line $ map per [ 3/8, 3/8, 1/4 ]
synco 2 per dur         = times dur $ line [ (times 4 $ per(3/16)), per(1/4)]
synco 3 per dur         = times dur $ line [ (times 10 $ per(3/32)), per(1/16)] 

