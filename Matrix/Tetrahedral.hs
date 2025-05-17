module Matrix.Tetrahedral where

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
e = aa*a

a = Mat [
    [ 0, 0, 1],
    [ 1, 0, 0],
    [ 0, 1, 0]]

b = Mat [
    [ 0, 0, 1],
    [-1, 0, 0],
    [ 0,-1, 0]]

c   = b*b*a*a
d   = a*a*b*b
aa  = a*a
bb  = b*b
cc  = c*c 
dd  = d*d
h   = b*a*a 
i   = a*b*a
j   = a*a*b

-- x, y, z = Reflection in Out(A_4)

inv = transpose 
iota    g x     = g * x * inv g
left    g x     = g * x 
right   g x     = x * inv g


l = Mat [
    [ 0,-1, 1],
    [ 1, 0,-1],
    [-1, 1, 0]]


z = Mat [
    [ 0, 0,-1],
    [ 0,-1, 0],
    [-1, 0, 0]]

z'= Mat [
    [ 0, 1, 0],
    [-1, 0, 0],
    [ 0, 0, 1]]

m = Mat [
    [ 0, 0, 1],
    [ 0,-1, 0],
    [ 1, 0, 0]]

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


