module Music.Songs where

import Euterpea
import Music.Music
import Music.Instrument
import Music.Percussion
import Music.KeyChord




{-- Music Modules --} 
module_8 n          = pSoundList k en n 50 58 [0..3]
module_12 n         = pSoundList (mute 50) en n 50 62 [0..3]  
module_1            = line $ map (padic (kchord 8) 0 1) [1,3,5,7,2,4,6,8,8,6,4,2,7,5,3,1]   -- Dur: 
module_2            = line $ map (padic kfsM 16 2) $ map (p_strict 3 0 8) [1..8]


{-- P-adic --}
seed                = 1
padic key h t p     = line $ pSoundList key (1/16) p 48 (60+h) [1..2^t]          --          Dur: (2^t)/16
padic' key h t p'   = line $ map(padic key h t)  $ map(p_strict p'   0 8) [1..8] -- :: [p]   Dur: (2^t)/2
padic'' key h t     = line $ map(padic' key h t) $ map(p_strict seed 0 8) [1..8] -- :: [p']  Dur: 2^(t+3)
padic''' key h p'   = line $ map(padic'' key h)  $ map(p_strict p'   0 4) [5..8] -- :: [t]   Dur: 2^(
padic'''' key h     = line $ map(padic''' key h) $ map(p_strict seed 0 8) [1..8] -- :: [p']  
padic''''' key p'   = line $ map(padic'''' key)  $ map(p_strict p'   0 8) [1..8] -- :: [h]   
padic'''''' key     = line $ map(padic''''' key) $ map(p_strict seed 0 8) [1..8] -- :: [p']  

{-- P-adic --}
seed2               = 3
padic2 key h t p    = line $ pSoundList key (1/16) p 53 (70+h) [1..2^t]          --          Dur: (2^t)/16
padic2' key h t p'  = line $ map(padic key h t)  $ map(p_strict p'      1 9) [1..8] -- :: [p]   Dur: (2^t)/2
padic2'' key h t    = line $ map(padic' key h t) $ map(p_strict seed2   2 8) [1..8] -- :: [p']  Dur: 2^(t+3)
padic2''' key h p'  = line $ map(padic'' key h)  $ map(p_strict p'      3 7) [5..8] -- :: [t]   Dur: 2^(
padic2'''' key h    = line $ map(padic''' key h) $ map(p_strict seed2   4 8) [1..8] -- :: [p']  
padic2''''' key p'  = line $ map(padic'''' key)  $ map(p_strict p'      0 8) [1..8] -- :: [h]   
padic2'''''' key    = line $ map(padic''''' key) $ map(p_strict seed2   0 8) [1..8] -- :: [p']  




good_melody_1       = line $ (...) map padic2''' kfsM 2 [0..7]
good_melody_2       = line $ (...) map padic2''' (kchord 11) 12 [0..7]  
r_good_melody_1     = line $ reverse $ lineToList good_melody_1 


{-- Egg Song --} 
egg_song_per        = rest 32 :+: rep (1024) (hand qn)
egg_song_vocal      = guitar 5 ( rest 16 :+: good_melody_1 )
egg_song_bass       = cello good_melody_1 
egg_song            = row [ egg_song_per, egg_song_vocal, egg_song_bass ]  

hoge_per            = egg_song_per
hoge_vocal          = guitar 5 ( rest 16 :+: good_melody_2 )
hoge_bass           = cello good_melody_2
hoge                = row [ hoge_vocal, hoge_bass ]


{-- Nature Sound --} 
rain                = times 10 (line [times 10(b 4(8/24)), b 4(3/24)] ):+: rest (17/12)     -- Dur: 36 
storm               = times 10 $ line $ map ((t (1/100)) . k) [1 .. 100]                    -- Dur: 10  


{-- The Clock Song --} 
long_vowel          = celesta   $ line $ pSoundList kcM (1/8) 7 36 60 [0..480]
short_vowel         = xylo      $ line $ pSoundList kcM  1    2 24 32 [0.. 48]
clock               = long_vowel :=: short_vowel


{-- Wind Bell --}
windBell1           = line [row[c 4(1/8),d 4(1/5)],row[d 4(1/7),e 4(1/8)],row[e 4(1/8),g 4(1/6)]] -- Dur: 107/210
windBell2           = line [a 4 qn, b 4 (1/5), cs 5 (1/12), d 5 (1/9), e 5 (1/7) ]       -- Dur: 248/315
windBell3           = line [g 5 qn, a 5 (1/7), b 5 qn, c 6 (1/5), d 6 (1/13)]            -- Dur: 837/910
windBell            = guitar 2 $ mix' 100 windBell1 windBell2 windBell3


{-- Beethoven No.9 --} 
bta                 = hn' [b 4, b 4, c 5, d 5] 
btb                 = hn' [d 5, c 5, b 4, a 4]
btc                 = hn' [g 4, g 4, a 4, b 4]
btd                 = [b 4 (3/4), a 4 qn, a 4 1] 
btd'                = [a 4 (3/4), g 4 qn, g 4 1]
bte                 = hn' [a 4, a 4, b 4, g 4]
btf                 = [a 4 hn, b 4 qn, c 5 qn, b 4 hn, g 4 hn]
btg                 = [a 4 hn, b 4 qn, c 5 qn, b 4 hn, a 4 hn]
bth                 = [g 4 hn, a 4 hn, d 4 1] 

bt1                 = concat [bta, btb, btc, btd]       -- dur: 8
bt2                 = concat [bta, btb, btc, btd']      -- dur: 8
bt3                 = concat [bte, btf, btg, bth]       -- dur: 8
bt                  = concat [bt1, bt2, bt3, bt2]       -- dur: 32
rbt                 = reverse bt
--beethoven9          = synth 1 (line bt :+: line rbt)  
beethoven9          = cello (line bt :+: line rbt)  


{-- Nodojiman --}
nodo_chime_1        = en' [c 5, b 4, a 4, g 4]
nodo_chime_2        = [c 5 qn, e 5 qn, d 5 hn] 
nodo_chime          = (tubular . line . concat) [nodo_chime_1, nodo_chime_1, nodo_chime_2]

nodo1               = [e 5 qn, d 5 qn, c 5 dqn, b 4 en]
nodo2               = en' [d 5, c 5, b 4, a 4] ++ [g 4 hn]
nodo2'              = [rest (5/8), e 4 en, f 4 en, fs 4 en] 
nodo3               = [g 4 dqn, a 4 sn, g 4 sn] ++ en' [fs 4, g 4, e 5, d 5]
nodo4               = [c 5 qn, b 4 den, d 5 sn] ++ en' [c 5, b 4, a 4, g 4]
nodo5               = [f 4 dqn, g 4 sn, f 4 sn] ++ en' [e 4,f 4, d 5, c 5]
nodo6               = [b 4 1] 
nodo7               = (.) map transpose (-2) nodo3
nodo8               = [b 4 qn, rest en, b 4 en, c 5 en, b 4 en]
nodo_opening        = tubular $ line nodo1 :+: (...) row map line [nodo2,nodo2'] 
                          :+: (...) line map line [nodo3,nodo4,nodo5,nodo6,nodo7,nodo8] 

{-- Chime --}
chime1              = [g 4 hn, b 4 hn, a 4 hn, d 4 hn] 
chime2              = [d 4 bn]
chime3              = reverse chime1
chime4              = [g 4 bn]
chime               = tieTwoLists chime1 chime2 ++ tieTwoLists chime3 chime4
bell n              = times n $ row [g 4 bn, d 4 bn, b 4 bn]
oclock n            = transpose (-2) $ tubular $ line $ chime ++ [bell n] 

{-- Clock and Beethoven9 (Original --} 
clock_and_beethoven = do 
        rnds      <-  randomSounds k (1/20) 2 36 72 160
        let vocal   = beethoven9 :+: rnds :+: transpose(5)beethoven9 :+: rnds in 
            let bass    = times 3 clock in 
                p $ row [ vocal, bass, synco 1 hand 48]

brk n = rep (4*n) (rest en) 

after_the_storm = p $ 
            row [ 
                    brk 8 :+: egg_song, 
                    rain :+: double storm :+: brk 16 :+: rain :+: double storm :+: brk 32 :+: rain :+: clock ] 
