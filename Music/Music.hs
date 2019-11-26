module Music.Music where 

import Euterpea 
import Control.DeepSeq
import System.Random

import Music.Instrument
import Music.KeyChord
import Music.Percussion

p       :: forall a. (Control.DeepSeq.NFData a, ToMusic1 a) => Music a -> IO ()
p x     = do
    playDev 2 $ line [ rest 1, x ]

b'                          =   (.)             -- B    combinator 
t x y                       =   y x             -- T    combinator 
(...)                       =   (.)(.)(.)       -- BBB  combinator

toNum                       =   fromInteger . toInteger 

unline                      =   lineToList
line'                       =   foldl1 (\x xs -> x :+: xs)      -- Euterpea.line
row                         =   foldl1 (\x xs -> x :=: xs)      -- Euterpea.chord 
rep     n x                 =   line' (replicate n x) 
times'  n x                 =   if n>1 then x :+: times' (n-1) x else x 
double                      =   times' 2
mix' repeat a b c           =   row $ (.) map times repeat [(a:+:b),(b:+:c),(c:+:a)]

k' n                        =   k n (1/(4 + (...) toNum mod n 19))

restrict low high n         =   low + mod n (high-low)
p_strict p low high         =   (restrict low high) . (p*) 
strict                      =   restrict 0 100 

twice f                     =   (.) f 
addDur                      =   t
hn'                         =   (.) map addDur hn  
qn'                         =   (.) map addDur qn 
en'                         =   (.) map addDur en 

toNote key dur              =   (addDur dur) . key
toNoteList                  =   (...) map toNote                       
pSoundList k dur p low high =   toNoteList k dur . map (p_strict p low high) 

tie (Prim(Note d(k,o))) 
    (Prim(Note e(l,p))) 
        | k==l, o==p        =   Prim (Note (d+e) (k,o))
tie x y | otherwise         =   error "'tie' needs two notes which has the same key"      
tieTwoLists l l'            =   let x:xs = reverse l in let y:ys = l' in 
                                reverse xs ++ [tie x y] ++ ys
tieTwoLines l l'            =   line $ tieTwoLists (lineToList l) (lineToList l')

insts                       = [MusicBox, SynthVoice, Celesta, TubularBells]
inst                        = instrument 

randomList low high            = do    
        num     <- randomIO                 :: IO Int 
        return ( randomRs (low,high) (mkStdGen num) :: [Int] )

randomSounds key dur p low high n    = do 
        r       <- randomList low high 
        return $ ( line . take n . pSoundList key dur p low high ) r


-- DRUM SOUND
drum1 = times 8 $ line $ [rep 8 $ perc LowTom (1/32), rep 6 $ perc LowTom en]
