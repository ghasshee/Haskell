module RenderArrow where 

import Graphics.UI.GLUT as GL 
import Complex

riemannize' (x:y:z) = 
    let r2 = x^2 + y^2 in
    [2*x/(1+r2), 2*y/(1+r2),(r2-1)/(r2+1)]
riemannize (C x y) = 
    let r2 = x^2 + y^2 in
    [2*x/(1+r2), 2*y/(1+r2),(r2-1)/(r2+1)]
c2a     (C x y) = [x,y,0]
instance Num a => Num [a] where
    [x,y,_] * [a,b,_] = [x*a-y*b,x*b+y*a,0]
    [x,y,z] + [a,b,c] = [x+a,y+b,z+c]
    [x,y,z] - [a,b,c] = [x-a,y-b,z-c]
a2c     [x,y,_] = C x y

mul k       [x,y,z] = [k*x,k*y,k*z]

render = renderPrimitive
gl :: Double -> GLdouble
gl  = id
v2p (Vertex3 x y z) = [x,y,z]
p2v [x,y,z]         = Vertex3 x y z

-- Line to Triabgle 
line2tri [p,q]  = [l_t p q, r_t p q, q]    
l_t p q         = (mib p q) + (l2l p q)     -- leftBottom  point of triangle
r_t p q         = (mib p q) + (l2l q p)     -- rightBottom point of triangle
l2l p q         = mul(1/16)(normal2d(q-p))  
mib p q         = p + (mul(7/8)(q-p))       -- midBottom point of the triange
normal2d [x,y,z]  = [-y,x,z]

-- Render Functions
renderLine col line = do
    render Lines $ do 
        color col
        mapM_ vertex $ map p2v line

renderArrow line = do
    render Lines $ mapM_ vertex $ map p2v line
    render Triangles $ mapM_ vertex $ map p2v $ line2tri line




