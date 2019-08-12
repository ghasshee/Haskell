-- Haskell School of Music, Chapter 4

import Euterpea
import Euterpea.Music

-- ===== Exercise 4.1 =====

-- 五等分の気持ち（サビ）

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = let f n = n d
              in line (map f ns)

harmonic4Note :: AbsPitch -> AbsPitch -> AbsPitch -> Dur -> Pitch -> Music Pitch
harmonic4Note i j k d p = note d p :=: note d (trans i p) :=: note d (trans j p) :=: note d (trans k p)

harmonic3Note :: AbsPitch -> AbsPitch -> Dur -> Pitch -> Music Pitch
harmonic3Note i = harmonic4Note i i

harmonic2Note :: AbsPitch -> Dur -> Pitch -> Music Pitch
harmonic2Note i = harmonic4Note i i i

hm12, hm5, hm4, hm3 :: PitchClass -> Octave -> Dur -> Music Pitch
hm12 pc o d     = harmonic2Note (-12) d (pc, o)
hm5 pc o d      = harmonic2Note (-5) d (pc, o)
hm4 pc o d      = harmonic2Note (-4) d (pc, o)
hm3 pc o d      = harmonic2Note (-3) d (pc, o)

hm7_12, hm7_10, hm5_9, hm3_8, hm4_11, hm6_9, hm3_6 :: PitchClass -> Octave -> Dur -> Music Pitch
hm7_12 pc o d   = harmonic3Note (-7) (-12) d (pc, o)
hm7_10 pc o d   = harmonic3Note (-7) (-10) d (pc, o)
hm5_9 pc o d    = harmonic3Note (-5) (-9) d (pc, o)
hm3_8 pc o d    = harmonic3Note (-3) (-8) d (pc, o)
hm4_11 pc o d   = harmonic3Note (-4) (-11) d (pc, o)
hm6_9 pc o d    = harmonic3Note (-6) (-9) d (pc, o)
hm3_6 pc o d    = harmonic3Note (-3) (-6) d (pc, o)

hm3_8_12, hm5_8_12 :: PitchClass -> Octave -> Dur -> Music Pitch
hm3_8_12 pc o d = harmonic4Note (-3) (-8) (-12) d (pc, o)
hm5_8_12 pc o d = harmonic4Note (-5) (-8) (-12) d (pc, o)

b38 = addDur qn [rest, hm12 B 2, hm12 Ds 3, hm12 E 3]
b39 = times 2 $ addDur en [hm12 E 3, e 3, hm5 E 4, e 3]
b40 = transpose (-3) b39
b41 = transpose (-7) b39
b42 = transpose (-5) b39
b43 = transpose (-7) b39
b44 = line [times 3 (hm12 Gs 2 en), hm12 Cs 3 qn, cs 3 en, hm5 Cs 4 en, cs 3 en]
b45 = line [hm12 A 3 qn, hm12 A 3 qn, hm12 A 3 en, a 3 en, hm5 A 4 en, a 3 en]
b46 = transpose 2 b45
b47 = b39
b48 = b40
b49 = b41
b50 = b42
b51 = b43
b52 = b44
b53 = times 8 (hm12 A 4 en)
b54 = times 4 (hm12 A 4 en) :+: times 4 (hm12 B 4 en) 
b55 = rest qn :+: times 6 (hm12 B 4 en)
b56 = hm12 E 3 en

v38 = addDur qn [rest, hm12 B 4, hm12 Ds 5, hm12 E 5]
v39 = line [  hm7_12 B 5 qn, hm5 E 5 en, hm5 E 5 qn, hm4 Ds 5 qn,    hm5 E 5 en]
v40 = line [  hm7_12 B 5 qn,     e 5 en,     e 5 qn,     ds 5 qn,        e 5 en]
v41 = line [  hm7_10 B 5 qn, hm3 E 5 en, hm3 E 5 qn, hm3 Fs 5 qn, hm5_9 Gs 5 en]
v42 = line [hm3_8_12 B 5 qn,     e 5 en,     e 5 qn,     fs 5 qn,       gs 5 en]
v43 = addDur qn [gs 5, hm4 Cs 5, ds 5, e 5]
v44 = line [hm3 B 4 en, b 4 en, fs 5 en, hm3_8 E 5 dqn, hm3 E 5 en, hm3 Fs 5 en]
v45 = line [hm4_11 Gs 5 dqn, fs 5 en,   hm6_9 Fs 5 hn]
v46 = line [  hm3_6 A 5 dqn,  b 5 en, hm5_8_12 B 5 hn]
v47 = v39
v48 = v40
v49 = v41
v50 = v42
v51 = v43
v52 = v44
v53_54 = times 4 (addDur en [a 5, gs 5, e 5]) :+: addDur en [a 5, gs 5, a 5, b 5]
v55 = enr :+: times 4 (e 5 en) :+: gs 4 en :+: hm3 Fs 5 qn
v56 = hm5 E 5 en

bass = line [b38, b39, b40, b41, b42, b43, b44, b45, b46, b47, b48 ,b49, b50, b51, b52, b53, b54, b55, b56]
voice = line [v38, v39, v40, v41, v42, v43, v44, v45, v46, v47, v48 ,v49, v50, v51, v52, v53_54, v55, v56]

gotobun :: Music Pitch
gotobun = tempo (175/120) (bass :=: voice)

-- Test music/code
test_4_1_a = play gotobun
