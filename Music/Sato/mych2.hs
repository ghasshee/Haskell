import Euterpea
import Euterpea.Music
-- ===== Exercise 3.14 =====
part :: Music Pitch
part = line $ map (\m -> m :+: m) [m1, m2, m3, m4]
    where   m1 = line [c 4 qn, d 4 qn, e 4 qn, c 4 qn]
            m2 = line [e 4 qn, f 4 qn, g 4 hn]
            m3 = line [g 4 en, a 4 en, g 4 en, f 4 en, e 4 qn, c 4 qn]
            m4 = line [c 4 qn, g 3 qn, c 4 hn]

roundVoice :: Music Pitch -> Dur -> [InstrumentName] -> Music Pitch
roundVoice part delay insts =   
    let f (idx, inst) = rest (delay * idx) :+: instrument inst part in 
    foldr1 (:=:) $ map f (zip [0..] insts)

insts           = [MusicBox, SynthVoice, Celesta, TubularBells]
fj              = roundVoice part bn insts
-- Test music/code
test_3_14_a     = play part
test_3_14_b     = play fj
