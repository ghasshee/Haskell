
import Graphics.UI.GLUT
import Data.IORef

cubeVertex :: [Vertex3 GLdouble]
cubeVertex = [ Vertex3 x y z | x <-[0,1], y<-[0,1], z<-[0,1] ]

cubeFace :: [(Int, Int, Int, Int)]
cubeFace = [ (0, 1, 3, 2)
           , (0, 1, 5, 4)
           , (2, 3, 7, 6)
           , (4, 5, 7, 6)
           , (0, 4, 6, 2)
           , (1, 5, 7, 3)
           ]

cubeColor :: [Color3 GLdouble]
cubeColor = [ Color3 0.5 0 0
            , Color3 0 1 0
            , Color3 0 0 1
            , Color3 1 1 0
            , Color3 1 0 1
            , Color3 0 1 1
            ]

display :: IORef GLdouble -> IORef GLdouble -> DisplayCallback
display rot1 rot2 = do
    clear [ColorBuffer, DepthBuffer]
    r1 <- get rot1
    r2 <- get rot2
    loadIdentity
    rotate r1 $ Vector3 1 0 0
    rotate r2 $ Vector3 0 1 0
    renderPrimitive Quads $ do
        mapM_ (draw cubeVertex) $ zip cubeFace cubeColor
    swapBuffers

draw :: [Vertex3 GLdouble] -> ((Int, Int, Int, Int), Color3 GLdouble) 
     -> IO ()
draw xs ((n,m,t,s), cl) = do
    color3d cl
    vertex3d (xs !! n)
    vertex3d (xs !! m)
    vertex3d (xs !! t)
    vertex3d (xs !! s)
        where
            color3d = color :: Color3 GLdouble -> IO ()
            vertex3d = vertex :: Vertex3 GLdouble -> IO ()

resize :: Size -> IO ()
resize s@(Size w h) = do
    viewport $= (Position 0 0,s)
    matrixMode $= Projection
    loadIdentity
    perspective 45.0 (w'/h') 1.0 100.0
    lookAt (Vertex3 0.5 0.5 (-5)) (Vertex3 0.5 0.5 0.5) (Vector3 0 1 0)
    matrixMode $= Modelview 0
        where
            w' = realToFrac w
            h' = realToFrac h

keyboard :: IORef GLdouble -> IORef GLdouble -> KeyboardCallback
keyboard rot1 rot2 c _ = do
    case c of
      'j' -> rot1 $~! (subtract 3)
      'k' -> rot1 $~! (+3)
      'h' -> rot2 $~! (subtract 3)
      'l' -> rot2 $~! (+3)
      'q' -> leaveMainLoop
      _ -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
    createWindow "Sample"
    clearColor $= Color4 1 1 1 1
    depthFunc $= Just Less
    rot1 <- newIORef 0
    rot2 <- newIORef 0
    displayCallback $= display rot1 rot2
    reshapeCallback $= Just resize
    keyboardCallback $= Just (keyboard rot1 rot2)
    idleCallback $= Just idle
    mainLoop
