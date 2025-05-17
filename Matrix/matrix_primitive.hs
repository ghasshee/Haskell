{--
 - class Matrix extends Num
 -
 - with 
 - det 
 - trans
 - inv
 - ...
 --}

import Data.Ratio
import Matrix
import Math

-- mini matrix
mini (Mat x) i j = 
    Mat [
            [(x!!xi!!xj) | xj<-([0..(j-1)]++[(j+1)..((len_col x)-1)])]
            | xi<-([0..(i-1)]++[(i+1)..((len_row x)-1)])
        ]

-- Determinant
det (Mat a)|((len_row a)/=(len_col a))  = error "non NxN matrix"
det (Mat [[x]])                         = x
det (Mat a)                             =
                    sum [
                            (a!!0!!n) * (-1)^n * (det(mini (Mat a) 0 n))
                            | n <- [ 0..((len_row a)-1)]
                        ]

-- Cofactor
cof (Mat a) i j = (-1)^(i+j) * (det(mini(Mat a)i j))
cm              = cof_mat 
cof_mat (Mat a) = 
        Mat [
                [ cof (Mat a) i j | i<-[0..((len_row a)-1)] ]
                | j <-[0..((len_col a)-1)]
            ]

-- Identity
identity n =
        Mat [
                [ delta i j | i <- [1..n]] 
                | j <- [1..n]
            ]

-- Elementary 
elementary_matrix n i j a b c d =
    Mat [
            [ 
                if      (k,l) == (i,i) then a
                else if (k,l) == (i,j) then b
                else if (k,l) == (j,i) then c
                else if (k,l) == (j,j) then d
                else  delta k l
                | k <- [1..n]
            ]
            | l <- [1..n]
        ]


--      1 2 3 4  ->  1 2 3 4
-- [a]  0 1 1 3  ->  0 1 1 3  [a]
-- [b]  0 1 2 0  ->  0 0 -1 3 [a]-[b]
--      0 9 9 3  ->  0 9 9 3
-- subtract [a] - [b]
sub a b (Mat x) =
    Mat [ 
            if i/=b 
                then x!!i 
                else (x!!a)-(x!!b) 
            | i<-[0..(length x - 1)]
        ]

---multiple c @ i-th row 
mul c i (Mat x) = 
    Mat [ 
            if k/=i 
                then x!!k 
                else map (* c) (x!!k) 
            | k<-[0..(length x - 1)]
        ]  

--Examples
a = [[1,2],[3,4]]
b = [[3,5],[9,10]]
aa =[[-1,3,5],[98,22,0],[2,33,89]]
aaa = [
    [1,2,3,4],
    [5,6,7,8],
    [3,0,9,8],
    [3,4,5,6] ]
p86eg = [
    [3,-2,5,1],
    [1,3,2,5],
    [2,-5,-1,4],
    [-3,2,3,2]]
p87ex1 = [
    [3,-5,2,10],
    [2,0,1,-3],
    [-2,3,5,2],
    [4,-2,-3,2]]
p87ex2 = [
    [3,2,5,-4],
    [-7,1,-8,6],
    [10,3,6,1],
    [2,5,4,3]]
p87ex3 = [
    [2,0,-3,1,4],
    [5,2,-1,-3,2],
    [-3,-1,0,4,1],
    [2,2,1,3,-2],
    [-2,-3,3,-2,4]]
p87ex4 = [
    [-1,-2,-3,4,1],
    [-2,3,4,1,3],
    [-5,-4,4,2,0],
    [3,2,-1,2,3],
    [2,3,3,0,-3]]
big = [
    [1,2,3,4,5,6,7,8,9,0],
    [3,1,1,4,45,5,3,5,4,4],
    [6,54,6,4,2,25,5,5,45,43],
    [3,4,4,4,2,2,3,4,2,1],
    [5,6,8,4,5,7,5,4,-1,4],
    [2,3,4,2,1,2,4,5,4,7],
    [3,4,5,54,4,4,6,8,42,4],
    [4,4,4,6,9,5,3,7,5,9],
    [5,4,2,1,8,9,6,6,5,3],
    [3,4,1,3,5,6,7,4,65,54]]

