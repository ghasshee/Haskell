module Animation where 

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU

flushInterval   = 0.001
timerInterval   = 1
setter x y      = x $= y  
i2f             = fromIntegral
v               = Vertex3
vec             = Vector3

width = 500
height = 500
backcolor = Color4 0 0 0 0 

call d x y z r ex ey ez ox oy oz t = do 
     d x y z t
     r ex ey ez ox oy oz t (Size width height) 
     addTimerCallback timerInterval $ call d x y z r ex ey ez ox oy oz (t+flushInterval)

action x y z t  = translate (vec (x t)(y t)(z t) :: Vector3 GLdouble)
eye x y z t     = v (x t)(y t)(z t) :: Vertex3 GLdouble
obj x y z t     = v (x t)(y t)(z t) :: Vertex3 GLdouble
upper           = vec 0 0 1   :: Vector3 GLdouble

display x y z t = do
    setter clearColor $ backcolor
    clear [ColorBuffer, DepthBuffer]
    loadIdentity                      -- Load Identy Matrix
    preservingMatrix $ renderObject Solid (Cube 1.0)
    preservingMatrix $ do
        action x y z t  
        renderObject Solid (Sphere' 0.7 10 10)
    swapBuffers

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
