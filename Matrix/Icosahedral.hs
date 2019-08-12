module Matrix.Icosahedral where

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
e = aaa*aa






{--
a = Mat [
    [ 1, 0, 0, 0, 0, 0],
    [ 0, 0, 0, 0, 0, 1],
    [ 0, 1, 0, 0, 0, 0],
    [ 0, 0, 1, 0, 0, 0],
    [ 0, 0, 0, 1, 0, 0],
    [ 0, 0, 0, 0, 1, 0]]

b = Mat [
    [ 0, 0, 1, 0, 0, 0],
    [ 0, 1, 0, 0, 0, 0],
    [ 0, 0, 0, 0, 1, 0],
    [ 0, 0, 0, 0, 0, 1],
    [ 0, 0, 0, 1, 0, 0],
    [ 1, 0, 0, 0, 0, 0]]
--}
a = Mat [
    [ 0, 0, 0, 1, 0],
    [ 0, 0, 1, 0, 0],
    [ 1, 0, 0, 0, 0],
    [ 0, 0, 0, 0, 1],
    [ 0, 1, 0, 0, 0]]

b = Mat [
    [ 0, 0, 1, 0, 0],
    [ 0, 0, 0, 0, 1],
    [ 0, 0, 0, 1, 0],
    [ 0, 1, 0, 0, 0],
    [ 1, 0, 0, 0, 0]]


c       = bbbb*a        -- = a*b*aaaa*bbbb
d       = aaaa*b
f       = b*aaaa
g       = a*bbbb
aa      = a*a;      bb      = b*b;      cc      = c*c      
dd      = d*d;      ff      = f*f;      gg      = g*g
aaa     = aa*a;     bbb     = bb*b;     ccc     = cc*c
ddd     = dd*d;     fff     = ff*f;     ggg     = gg*g
aaaa    = aa*aa;    bbbb    = bb*bb;    cccc    = cc*cc
dddd    = dd*dd;    ffff    = ff*ff;    gggg    = gg*gg

ab=a*b; ac=a*c; ad=a*d; af=a*f; ag=a*g;
ba=b*a; bc=b*c; bd=b*d; bf=b*f; bg=b*g;
ca=c*a; cb=c*b; cd=c*d; cf=c*f; cg=c*g;
da=d*a; db=d*b; dc=d*c; df=d*f; dg=d*g;
fa=f*a; fb=f*b; fc=f*c; fd=f*d; fg=f*g;
ga=g*a; gb=g*b; gc=g*c; gd=g*d; gf=g*f;


aab     = a*a*b
aba     = a*b*a
abb     = a*b*b
baa     = b*a*a
bab     = b*a*b
bba     = b*b*a


h   = baa     -- aaa*bbbb
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




-- ab - ba 
-- [0 -2  0]
-- [0  0  2]
-- [0  0  0]
--
--   = [0 -2  0  0  0  2  0  0  0]
--  
-- a = [0  0  1  1  0  0  0  1  0]


