import Complex
import Matrix

mult a (Mat b) = Mat (fmap (fmap (*a)) b)

i  = C 0 1
o  = C 0 0
_i = C 0(-1)
l  = C 1 0
_l = C(-1)0
_2 = C(-2)0

a = Mat [
    [ o, l, o],
    [ l, o, o],
    [ o, o, o]]
b = Mat [
    [ o,_i, o],
    [ i, o, o],
    [ o, o, o]]
c = Mat [
    [ l, o, o],
    [ o,_l, o],
    [ o, o, o]]
d = Mat [
    [ o, o, l],
    [ o, o, o],
    [ l, o, o]]
e = Mat [
    [ o, o,_i],
    [ o, o, o],
    [ i, o, o]]
f = Mat [
    [ o, o, o],
    [ o, o, l],
    [ o, l, o]]
g = Mat [
    [ o, o, o],
    [ o, o,_i],
    [ o, i, o]]
h = mult (C(sqrt 3)0) $ Mat [
    [ l, o, o],
    [ o, l, o],
    [ o, o,_2]]


aa = a * a
ba = b * a
ca = c * a
da = d * a
ea = e * a
fa = f * a
ga = g * a
ha = h * a
ab = a * b
bb = b * b
cb = c * b
db = d * b
eb = e * b
fb = f * b
gb = g * b
hb = h * b
ac = a * c
bc = b * c
cc = c * c
dc = d * c
ec = e * c
fc = f * c
gc = g * c
hc = h * c
ad = a * d
bd = b * d
cd = c * d
dd = d * d
ed = e * d
fd = f * d
gd = g * d
hd = h * d
ae = a * e
be = b * e
ce = c * e
de = d * e
ee = e * e
fe = f * e
ge = g * e
he = h * e
af = a * f
bf = b * f
cf = c * f
df = d * f
ef = e * f
ff = f * f
gf = g * f
hf = h * f
ag = a * g
bg = b * g
cg = c * g
dg = d * g
eg = e * g
fg = f * g
gg = g * g
hg = h * g
ah = a * h
bh = b * h
ch = c * h
dh = d * h
eh = e * h
fh = f * h
gh = g * h
hh = h * h

