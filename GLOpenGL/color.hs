import Graphics.UI.GLUT

type G = GLfloat

type P = (G,G,G) 
data Shape = Box P P
            |Ball P G
            |Quad P P P P P P P P deriving Show

-- quad :: Box -> Quad
-- quad (Box (x,y,z)(u,v,w)) = 
--    Quad (x,y,z)(x,y,w)(x,v,z)(x,v,w)
--         (u,y,z)(u,y,w)(u,v,z)(u,v,w)


rect0 :: G -> G -> G ->[P]
rect0 = (\x y z -> [(u,v,0)| u<-[0,x],v<-[0,y]])
twist = (\[x,y,z,w] -> [x,y,w,z])

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "color"
    displayCallback $= display
    mainLoop


display :: DisplayCallback
display = do
    let color3f r g b = color $ 
            Color3 r g (b:: GLfloat)
        vertex3f (x,y,z) = vertex $ 
            Vertex3 x y z
        rect3f x y z = mapM_ vertex3f $ 
            twist $ rect0 x y z
    clear [ColorBuffer]
    renderPrimitive Quads $ do
        color3f 1 0 0
        rect3f  0.5 0.5 0

        color3f 0 1 0
        rect3f  0.5 (-0.5) 0

        color3f 0 0 1
        rect3f  (-0.5) (-0.5) 0

        color3f 1 0 1
        rect3f  (-0.5) 0.5 0
        
    flush


