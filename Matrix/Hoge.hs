module Matrix.Hoge where

import Matrix.Matrix
-------------------- an expample usage -------------------- 
-- Now we think of
-- * Symmetry Group S_4 
-- * Alternation Group A_4 
-- A_4 is generated just by a & b
-- S_4 is generated just by a, b & z

-- the Matrix is like

--                   [0 0 1]   
-- [x'y'z'] = [x y z][1 0 0]   
--                   [0 1 0]   

-- here x,y or z axis implies each axis of 3 180˚rotations 
-----------------------------------------------------------

-----------------------------------
--  A_4 is a alternation group on a underlying set {P_1,P_2,P_3,P_4} 
-- 
--  where 
--      P_1 := ( -1,  1,  1)
--      P_2 := (  1, -1,  1)
--      P_3 := (  1,  1, -1)
--      P_4 := ( -1, -1, -1)
--
--------------------------------

o = Mat [
    [ 0, 0, 0, 0],
    [ 0, 0, 0, 0],
    [ 0, 0, 0, 0],
    [ 0, 0, 0, 0]]

sa x = x + Mat [
    [ 0, 0, 1, 0],
    [ 1, 0, 0, 0],
    [ 0, 1, 0, 0],
    [ 0, 0, 0, 1]]
sb x = x + Mat [
    [ 0, 0, 0, 1],
    [ 0, 1, 0, 0],
    [ 1, 0, 0, 0],
    [ 0, 0, 1, 0]]

a = sa o
b = sb o 



c       = ab*ab        -- = a*b*aaaa*bbbb
d       = ba*ba
f       = e
g       = e

e   = aaa
h   = baa    
i   = aba     
j   = aab
k   = abb
l   = bab
m   = bba

-- x, y, z = Reflection in Out(A_4)

inv = transpose 
iota    g x     = g * x * inv g
left    g x     = g * x 
right   g x     = x * inv g




z = Mat [
    [ 0, 0,-1],
    [ 0,-1, 0],
    [-1, 0, 0]]

z'= Mat [
    [ 0, 1, 0],
    [-1, 0, 0],
    [ 0, 0, 1]]


n = Mat [
    [ 0, 0,-1],
    [ 0,-1, 0],
    [-1, 0, 0]]

za = z * a
zb = z * b
zc = z * c
zd = z * d
zaa = z * aa
zbb = z * bb
zcc = z * cc
zdd = z * dd
zh = z * h
zi = z * i
zj = z * j






-- ABBREVIATIONS aa ~ gg
aa=a*a; ab=a*b; ac=a*c; ad=a*d; af=a*f; ag=a*g;
ba=b*a; bb=b*b; bc=b*c; bd=b*d; bf=b*f; bg=b*g;
ca=c*a; cb=c*b; cc=c*c; cd=c*d; cf=c*f; cg=c*g;
da=d*a; db=d*b; dc=d*c; dd=d*d; df=d*f; dg=d*g;
fa=f*a; fb=f*b; fc=f*c; fd=f*d; ff=f*f; fg=f*g;
ga=g*a; gb=g*b; gc=g*c; gd=g*d; gf=g*f; gg=g*g;
-- ABBREVIATIONS aaa ~ ggg
aaa=aa*a;   aab=aa*b;   aac=aa*c;   aad=aa*d;   aaf=aa*f;   aag=aa*g
aba=ab*a;   abb=ab*b;   abc=ab*c;   abd=ab*d;   abf=ab*f;   abg=ab*g
aca=ac*a;   acb=ac*b;   acc=ac*c;   acd=ac*d;   acf=ac*f;   acg=ac*g
ada=ad*a;   adb=ad*b;   adc=ad*c;   add=ad*d;   adf=ad*f;   adg=ad*g
afa=af*a;   afb=af*b;   afc=af*c;   afd=af*d;   aff=af*f;   afg=af*g
aga=ag*a;   agb=ag*b;   agc=ag*c;   agd=ag*d;   agf=ag*f;   agg=ag*g
baa=ba*a;   bab=ba*b;   bac=ba*c;   bad=ba*d;   baf=ba*f;   bag=ba*g
bba=bb*a;   bbb=bb*b;   bbc=bb*c;   bbd=bb*d;   bbf=bb*f;   bbg=bb*g
bca=bc*a;   bcb=bc*b;   bcc=bc*c;   bcd=bc*d;   bcf=bc*f;   bcg=bc*g
bda=bd*a;   bdb=bd*b;   bdc=bd*c;   bdd=bd*d;   bdf=bd*f;   bdg=bd*g
bfa=bf*a;   bfb=bf*b;   bfc=bf*c;   bfd=bf*d;   bff=bf*f;   bfg=bf*g
bga=bg*a;   bgb=bg*b;   bgc=bg*c;   bgd=bg*d;   bgf=bg*f;   bgg=bg*g
caa=ca*a;   cab=ca*b;   cac=ca*c;   cad=ca*d;   caf=ca*f;   cag=ca*g
cba=cb*a;   cbb=cb*b;   cbc=cb*c;   cbd=cb*d;   cbf=cb*f;   cbg=cb*g
cca=cc*a;   ccb=cc*b;   ccc=cc*c;   ccd=cc*d;   ccf=cc*f;   ccg=cc*g
cda=cd*a;   cdb=cd*b;   cdc=cd*c;   cdd=cd*d;   cdf=cd*f;   cdg=cd*g
cfa=cf*a;   cfb=cf*b;   cfc=cf*c;   cfd=cf*d;   cff=cf*f;   cfg=cf*g
cga=cg*a;   cgb=cg*b;   cgc=cg*c;   cgd=cg*d;   cgf=cg*f;   cgg=cg*g
daa=da*a;   dab=da*b;   dac=da*c;   dad=da*d;   daf=da*f;   dag=da*g
dba=db*a;   dbb=db*b;   dbc=db*c;   dbd=db*d;   dbf=db*f;   dbg=db*g
dca=dc*a;   dcb=dc*b;   dcc=dc*c;   dcd=dc*d;   dcf=dc*f;   dcg=dc*g
dda=dd*a;   ddb=dd*b;   ddc=dd*c;   ddd=dd*d;   ddf=dd*f;   ddg=dd*g
dfa=df*a;   dfb=df*b;   dfc=df*c;   dfd=df*d;   dff=df*f;   dfg=df*g
dga=dg*a;   dgb=dg*b;   dgc=dg*c;   dgd=dg*d;   dgf=dg*f;   dgg=dg*g
faa=fa*a;   fab=fa*b;   fac=fa*c;   fad=fa*d;   faf=fa*f;   fag=fa*g
fba=fb*a;   fbb=fb*b;   fbc=fb*c;   fbd=fb*d;   fbf=fb*f;   fbg=fb*g
fca=fc*a;   fcb=fc*b;   fcc=fc*c;   fcd=fc*d;   fcf=fc*f;   fcg=fc*g
fda=fd*a;   fdb=fd*b;   fdc=fd*c;   fdd=fd*d;   fdf=fd*f;   fdg=fd*g
ffa=ff*a;   ffb=ff*b;   ffc=ff*c;   ffd=ff*d;   fff=ff*f;   ffg=ff*g
fga=fg*a;   fgb=fg*b;   fgc=fg*c;   fgd=fg*d;   fgf=fg*f;   fgg=fg*g
gaa=ga*a;   gab=ga*b;   gac=ga*c;   gad=ga*d;   gaf=ga*f;   gag=ga*g
gba=gb*a;   gbb=gb*b;   gbc=gb*c;   gbd=gb*d;   gbf=gb*f;   gbg=gb*g
gca=gc*a;   gcb=gc*b;   gcc=gc*c;   gcd=gc*d;   gcf=gc*f;   gcg=gc*g
gda=gd*a;   gdb=gd*b;   gdc=gd*c;   gdd=gd*d;   gdf=gd*f;   gdg=gd*g
gfa=gf*a;   gfb=gf*b;   gfc=gf*c;   gfd=gf*d;   gff=gf*f;   gfg=gf*g
gga=gg*a;   ggb=gg*b;   ggc=gg*c;   ggd=gg*d;   ggf=gg*f;   ggg=gg*g

