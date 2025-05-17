{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-} 
module Three where 


import Graphics.UI.GLUT
import Complex_

type Three = (GLdouble,GLdouble,GLdouble) 

instance Num Three where
    (x,y,_) * (a,b,_)           =   (x*a-y*b,x*b+y*a,0)
    (x,y,z) + (a,b,c)           =   (x+a,y+b,z+c)
    (x,y,z) - (a,b,c)           =   (x-a,y-b,z-c)
    fromInteger n               =   (fromInteger n,0,0) 
    abs (x,y,z)                 =   (abs x,abs y,abs z)

mul k           (x,y,z)         =   (k*x,k*y,k*z)
fromVec     (Vector3 x y z)     =   (x,y,z) 
toVec           (x,y,z)         =   Vector3 x y z 
fromVtx     (Vertex3 x y z)     =   (x,y,z)
toVtx           (x,y,z)         =   Vertex3 x y z
fromComplex     (x,y)           =   (x,y,0)
toComplex       (x,y,_)         =   (x,y) :: Complex 


