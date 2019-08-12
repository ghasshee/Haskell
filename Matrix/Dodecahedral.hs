module Matrix.Dodecahedral where

import Matrix.Matrix
import Data.Set


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

data Root = 
          Root1
        | Root2
        | Root3
        | Root5
        | Root6
        | Root10
        | Root15
        | Root30 deriving (Ord,Eq)

instance Show Root where
    show Root1 = ""
    show Root2 = "!2"
    show Root3 = "!3"
    show Root5 = "!5"
    show Root6 = "!6"
    show Root10 = "!10"
    show Root15 = "!15"
    show Root30 = "!30"
    


data R = R Int Root

instance Ord R where
    (R m r)  > (R n s) = r > s 
instance Eq R where
    (R m r) == (R n s) = r == s && m == n
instance Show R where
    show (R m r) = show m ++ show r


plus :: [R] -> [R] -> [R]
plus [] [] = [R 0 Root1]
plus a [] = a
plus [] a = a 
plus ((R m r):xs) ((R n s):ys) | r==s   = (R(m+n)r):(plus xs ys)
plus ((R m r):xs) ((R n s):ys) | r<s    = (R m r):(plus xs ((R n s):ys))
plus ((R m r):xs) ((R n s):ys) | r>s    = (R n s):(plus ((R m r):xs) ys)

mult (R m r) (R n s) | r>s      = mult (R n s)(R m r)
mult (R m Root1) (R n Root1)    = R (1*n*m) Root1
mult (R m Root2) (R n Root2)    = R (2*n*m) Root2 
mult (R m Root3) (R n Root3)    = R (3*n*m) Root3 
mult (R m Root5) (R n Root5)    = R (5*n*m) Root5 
mult (R m Root6) (R n Root6)    = R (6*n*m) Root6 
mult (R m Root10) (R n Root10)  = R (10*n*m) Root10 
mult (R m Root15) (R n Root15)  = R (15*n*m) Root15 
mult (R m Root30) (R n Root30)  = R (30*n*m) Root30 
mult (R m Root1) (R n r)        = R (m*n) r
mult (R m Root2) (R n Root3)    = R (m*n) Root6
mult (R m Root2) (R n Root5)    = R (m*n) Root10
mult (R m Root2) (R n Root6)    = R (2*m*n) Root3
mult (R m Root2) (R n Root10)   = R (2*m*n) Root5
mult (R m Root2) (R n Root15)   = R (m*n) Root30
mult (R m Root2) (R n Root30)   = R (2*m*n) Root15
mult (R m Root3) (R n Root5)    = R (m*n) Root15
mult (R m Root3) (R n Root6)    = R (3*m*n) Root2
mult (R m Root3) (R n Root10)   = R (m*n) Root30
mult (R m Root3) (R n Root15)   = R (3*m*n) Root15
mult (R m Root3) (R n Root30)   = R (3*m*n) Root10
mult (R m Root5) (R n Root6)    = R (m*n) Root30
mult (R m Root5) (R n Root10)   = R (5*m*n) Root2
mult (R m Root5) (R n Root15)   = R (5*m*n) Root3
mult (R m Root5) (R n Root30)   = R (5*m*n) Root6
mult (R m Root6) (R n Root10)   = R (2*m*n) Root15
mult (R m Root6) (R n Root15)   = R (3*m*n) Root10
mult (R m Root6) (R n Root30)   = R (6*m*n) Root5
mult (R m Root10) (R n Root15)  = R (5*m*n) Root6
mult (R m Root10) (R n Root30)  = R (10*m*n) Root3
mult (R m Root15) (R n Root30)  = R (15*m*n) Root2


mmult a [] = [R 0 Root1] 
mmult [] a = [R 0 Root1]
mmult (x:xs) l = (fmap (mult x) l) + (mmult xs l)  


instance Num [R] where
    a + b   = 
        let a' = toList(fromList a) in
        let b' = toList(fromList a) in
        plus a' b'
    a * b   = 
        let a' = toList(fromList a) in
        let b' = toList(fromList a) in
        mmult a' b'



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


