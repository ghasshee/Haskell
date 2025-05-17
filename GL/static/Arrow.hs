import Graphics.UI.GLUT
import Complex 

camera = [0,0,1]
target = [0,0,0]
up     = [0,1,0]

z_z2_z3 z   = (z - z*z - z*z*z) / (C 5 0)
fish z      = (C (-1) 0) + (C 0 (-1)) * (z - (C 0.5 (-0.5))) * (z - (C (-1) 0))  
z2_1 z      = (z - (C 1 0))*(z + (C 1 0) )
eg01 z      = (z - (C 0 1)) * (z - (C 1 0))

f01 z       = acos z
f02 z       = z 
f03 z       = cos z  
f04 z       = exp z 
f05 z       =  (z -(C 0 1)) * (z -(C 0.5 0.8)  )
f06 z       = z^2 + z
f07 z       = - ( z - (C 0 1) ) / (C 2 0) 
f08 z       = z^2 + 1
__f         = f04
_f          = (zoomdown 2) . __f  -- .reciprocal   



zoomdown n z = z / (C n 0)
zoomup   n z = z * (C n 0) 

f  z = (fromComplex. _f .toComplex) z 


g [x,y,z]                   = [x,y,z]
gradient2d                  = id
hamilton2d                  = normal2d
drawField field f [x,y,z]   = [x,y,z] - (mul 0.2 (field (f [x,y,z]))) 

vec [x,y,z]                 = Vector3 x y z 

main                        :: IO ()
main                        = do 
    (_prog, _args)              <- getArgsAndInitialize
    initialWindowSize           $= Size 800 800
    _window                     <- createWindow "arrow"
    displayCallback             $= display
    mainLoop


display' :: DisplayCallback
display' = do
    clear [ColorBuffer]
    -- lookAt (toVtx $ map toGL camera) (toVtx $ map toGL target) (vec $ map toGL up)
    displayField hamilton2d g
    -- displayField gradient2d g 
    flush

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    -- lookAt (toVtx $ map toGL camera) (toVtx $ map toGL target) (vec $ map toGL up)
    displayImage 0 (action f (xlines 0.05 1 0.05 0.005)) 
    displayImage 0 (action f (ylines 0.05 1 0.05 0.005)) 
    flush

p2l          field f p      =   [drawField field f p, p]
displayField field f        =   mapM_ ( renderArrow . p2l field f ) gridPts 
-- displayImage       f        =   mapM_ ( renderLine (c 1 0 0 0) ) ( action f (grid 1 0.05 0.005))
displayImage     n []       = do return ()
displayImage     n (ln:lns) = do 
    renderLine (c 0.5 (0.0001*n) 0) ln
    displayImage (n+1) lns



c                           ::  GLdouble -> GLdouble -> GLdouble -> Color3 GLdouble
c r g b                     =   Color3 r g (toGL b)  

gridPts                     ::  [[GLdouble]]
gridPts                     =   [ map toGL [x,y,0] | x<-[-1,-0.95..1], y<-[-1,-0.95..1] ]

-- action f grid               = fmap (fmap (riemannize' . f)) grid 
action                      ::  (Functor g, Functor h) => (a -> b) -> h(g a) -> h(g b)     
action f grid               =   fmap (fmap (f)) grid 

grid                        ::  GLdouble -> GLdouble -> GLdouble -> GLdouble -> [[[GLdouble]]]
grid   a b itval ε            =   xlines a b itval ε ++ ylines a b itval ε 

xlines                      ::  GLdouble -> GLdouble -> GLdouble -> GLdouble -> [[[GLdouble]]]
xlines a b itval ε            =   pts2lines xlines_pts (map (translate' [0.005,0,0]) xlines_pts) 
    where   xlines_pts      =   [[x,y,0] | x<-[a,a+ε..b], y<-[a,a+itval..b]]
ylines                      ::  GLdouble -> GLdouble -> GLdouble -> GLdouble -> [[[GLdouble]]]
ylines a b itval ε            =   pts2lines ylines_pts (map (translate' [0,0.005, 0]) ylines_pts) 
    where   ylines_pts      =   [[y,x,0] | x<-[a,a+ε..b], y<-[a,a+itval..b]]

pts2lines starts ends       =   fmap anzip (zip starts ends)
    where anzip (s,t)       =   [s,t]

translate'                  ::  [GLdouble] -> [GLdouble] -> [GLdouble] 
translate' [x,y,z] [a,b,c]  =   [a,b,c] + [x,y,z] 



riemannize' [x,y,z]         =   [2*x/(1+r2), 2*y/(1+r2),(r2-1)/(r2+1)]
    where   r2              =   x^2 + y^2 

riemannize (C x y)          =   [2*x/(1+r2), 2*y/(1+r2),(r2-1)/(r2+1)]
    where   r2              =   x^2 + y^2 

fromComplex     (C x y)     =   [x,y,0]
toComplex     [x,y,_]       =   C x y

mul k   [x,y,z]             =   [k*x,k*y,k*z]
instance Num a => Num [a] where
    [x,y,_] * [a,b,_]       =   [x*a-y*b,x*b+y*a,0]
    [x,y,z] + [a,b,c]       =   [x+a,y+b,z+c]
    [x,y,z] - [a,b,c]       =   [x-a,y-b,z-c]


render                      =   renderPrimitive
toGL                        ::  Double -> GLdouble
toGL                        =   id
fromVtx (Vertex3 x y z)         =   [x,y,z]
toVtx [x,y,z]                 =   Vertex3 x y z

-- Line to Triangle 
line2tri [start,end]        = [l_t start end, r_t start end, end]    
l_t p q                     = (mib p q) + (l2l p q)     -- leftBottom  point of triangle
r_t p q                     = (mib p q) + (l2l q p)     -- rightBottom point of triangle
l2l p q                     = mul(1/16)(normal2d(q-p))  
mib p q                     = p + (mul(7/8)(q-p))       -- midBottom point of the triange
normal2d [x,y,z]            = [-y,x,z]

-- Render Functions
renderLine                  :: (Color c, VertexComponent line) => c -> [[line]] -> IO()
renderLine c line           = do
    render Lines $ do 
        color c
        mapM_ vertex $ map toVtx line

renderArrow line = do
    render Lines        $ mapM_ vertex $ map toVtx $ line
    render Triangles    $ mapM_ vertex $ map toVtx $ line2tri line







{--
 - lines_grid f      =
    map anzip (zip (map rf start) (map rf end))
        where
            start = grid 3 0.05 0.005 0 
            end   = map (transpose [
            rf = riemannize' . f 
            anzip (s,t) = [s,t] 

grid n itval ε =
    [ map toGL [x,y,0] | x <- [-n+ε,-n+ε+ε .. n+ε], y <- [-n,-n+itval .. n]] ++
    [ map toGL [y,x,0] | x <- [-n+ε,-n+ε+ε .. n+ε], y <- [-n,-n+itval .. n]] --}

