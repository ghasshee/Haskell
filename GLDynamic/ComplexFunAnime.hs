import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU
import Complex 

camera  :: [GLdouble]
camera  = [0,0,1]
target  :: [GLdouble]
target  = [0,0,0]
up      :: [GLdouble] 
up      = [0,1,0]

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
f z         = ( power_i z ) *z 

modify f    = fromComplex . zoomdown 2 . f . toComplex 
zoomdown n z    = z / (C n 0)
zoomup   n z    = z * (C n 0) 

c                           ::  GLdouble -> GLdouble -> GLdouble -> Color3 GLdouble
c r g b                     =   Color3 r g (toGL b)  

-- mapfun f grid               = fmap (fmap (riemannize' . f)) grid 
mapfun                      ::  (Functor g, Functor h) => (a -> b) -> h(g a) -> h(g b)     
mapfun f grid               =   fmap (fmap (f)) grid 

grid                        ::  (GLdouble,GLdouble) -> (GLdouble,GLdouble) -> GLdouble -> GLdouble -> [[[GLdouble]]]
grid   a b itval ε          =   concat $ map anzip $ zip(xlines a b itval ε)(ylines a b itval ε)
    where anzip (x,y)       = [x,y]

xlines                      ::  (GLdouble,GLdouble) -> (GLdouble,GLdouble) -> GLdouble -> GLdouble -> [[[GLdouble]]]
xlines (a,b)(c,d) itval ε   =   pts2lines xlines_pts (map (translate' [0.005,0,0]) xlines_pts) 
    where   xlines_pts      =   [[x,y,0] | x<-[a,a+ε..c], y<-[b,b+itval..d]]
ylines                      ::  (GLdouble,GLdouble) -> (GLdouble,GLdouble) -> GLdouble -> GLdouble -> [[[GLdouble]]]
ylines (a,b)(c,d) itval ε   =   pts2lines ylines_pts (map (translate' [0,0.005, 0]) ylines_pts) 
    where   ylines_pts      =   [[x,y,0] | x<-[a,a+itval..c], y<-[b,b+ε..d]]

pts2lines starts ends       =   fmap anzip (zip starts ends)
    where anzip (s,t)       =   [s,t]

translate'                  ::  [GLdouble] -> [GLdouble] -> [GLdouble] 
translate' [x,y,z] [a,b,c]  =   [a,b,c] + [x,y,z] 

riemannize'                 =   riemannize . toComplex
riemannize (C x y)          =   [2*x/(1+r2), 2*y/(1+r2),(r2-1)/(r2+1)]
    where   r2              =   x^2 + y^2 

fromComplex     (C x y)     =   [x,y,0]
toComplex     [x,y,_]       =   C x y

mul k   [x,y,z]             =   [k*x,k*y,k*z]
instance Num a => Num [a] where
    [x,y,_] * [a,b,_]       =   [x*a-y*b,x*b+y*a,0]
    [x,y,z] + [a,b,c]       =   [x+a,y+b,z+c]
    [x,y,z] - [a,b,c]       =   [x-a,y-b,z-c]
    fromInteger n           =   [fromInteger n,0,0] 
    abs [x,y,z]             =   [abs x,abs y,abs z]

render                      =   renderPrimitive
toGL                        ::  Double -> GLdouble
toGL                        =   id
fromVtx (Vertex3 x y z)     =   [x,y,z]
toVtx [x,y,z]               =   Vertex3 x y z
toVec [x,y,z]               =   Vector3 x y z 

-- Render Functions
renderLine                  :: (Color c, VertexComponent line) => c -> [[line]] -> IO()
renderLine c line           = do 
    render Lines $ do 
        color c
        mapM_ vertex $ map toVtx line
 


-- Motion of A Ball
x t = cos (2*t*t*t)
y t = sin (3*t*t*t)
z t = (x t)*(y t)

-- Viewpoint
ex t = 0 --4 * cos(t/20)
ey t = 3 --3 * sin(t/20)
ez t = 10

-- The Point Looking At
ox t = 0.0
oy t = 0.0
oz t = 0.0

main :: IO ()
main = do
  setter initialDisplayMode         $ [RGBAMode, DoubleBuffered, WithDepthBuffer] 
  setter initialWindowSize          $ (Size width height)
  getArgsAndInitialize
  createWindow                      "OpenGL GLUT at Haskell"
  setter displayCallback            $ display x y z 0
  setter reshapeCallback            $ Just (reshape ex ey ez ox oy oz 0)     
  addTimerCallback timerInterval    $ call display x y z reshape ex ey ez ox oy oz 0 
  mainLoop

main                        :: IO ()
main                        = do 
    (_prog, _args)              <- getArgsAndInitialize
    initialWindowSize           $= Size 800 800
    _window                     <- createWindow "arrow"
    displayCallback             $= display
    mainLoop

display x y z t = do
    setter clearColor $ backcolor
    clear [ColorBuffer, DepthBuffer]
    loadIdentity                      -- Load Identy Matrix
    preservingMatrix $ renderObject Solid (Cube 1.0)
    preservingMatrix $ do
        action x y z t  
        renderObject Solid (Sphere' 0.7 10 10)
    swapBuffers

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    lookAt (toVtx camera) (toVtx target) (toVec up)
    displayImage 1 1 0 0(mapfun (modify f) (grid(0.05,0.05)(1,1)0.05 0.005)) 
    displayImage 0 1 1 0(mapfun (modify f) (grid(0.05,-1)(1,-0.05)0.05 0.005)) 
    displayImage 1 0 1 0(mapfun (modify f) (grid(-1,0.05)(-0.05,1)0.05 0.005)) 
    displayImage 1 1 1 0(mapfun (modify f) (grid(-1,-1)(-0.05,-0.05)0.05 0.005)) 
    flush

displayImage     r g b n []       = do  return ()
displayImage     r g b n (ln:lns) = do  renderLine (c(r-0.0001*n)(g-0.0001*n)(b-0.0001*n)) ln
                                        displayImage r g b (n+1) lns



flushInterval   = 0.001
timerInterval   = 1
setter x y      = x $= y  
i2f             = fromIntegral
v               = Vertex3
vec             = Vector3

width           = 500
height          = 500
backcolor       = Color4 0 0 0 0 

call d x y z r ex ey ez ox oy oz t = do 
     d x y z t
     r ex ey ez ox oy oz t (Size width height) 
     addTimerCallback timerInterval $ call d x y z r ex ey ez ox oy oz (t+flushInterval)

action x y z t  = translate (vec (x t)(y t)(z t) :: Vector3 GLdouble)
eye x y z t     = v (x t)(y t)(z t) :: Vertex3 GLdouble
obj x y z t     = v (x t)(y t)(z t) :: Vertex3 GLdouble
upper           = vec 0 0 1   :: Vector3 GLdouble

cam_fov  = 30
cam_aspe = i2f width /i2f height 
cam_near = 0.1
cam_far  = 100.0

light0_pos          = Vertex4 (-6.0) 2.0 6.0 5.0
light0_diffu        = Color4 1.0 1.0 1.0 1.0
light0              = Enabled

material_diffuse    = Color4 1.0 1.0 1.0 1.0

reshape ex ey ez ox oy oz t size@(Size w h) = do
    setter viewport   $ (Position 0 0, size)
    setter matrixMode $ Projection
    loadIdentity
    perspective cam_fov cam_aspe cam_near cam_far
    lookAt (eye ex ey ez t) (obj ox oy oz t) upper
    setter matrixMode $ Modelview 0
    -- set Light
    setter lighting                           $ Enabled
    setter (position (Light 0))               $ light0_pos
    setter (diffuse  (Light 0))               $ light0_diffu
    setter (light    (Light 0))               $ light0
    setter (materialDiffuse FrontAndBack)     $ material_diffuse
    -- enable Depth Buffer
    setter depthFunc              $ Just Less



