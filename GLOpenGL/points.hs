import Graphics.UI.GLUT

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [( sin (2*pi*k/12), cos (2*pi*k/12), 0 ) | k <- [1..12] ]

myQuad :: [(GLfloat,GLfloat,GLfloat)]
myQuad = [(x,y,0) | x<-[0,1] , y<-[0,1]]

main :: IO ()
main = do
    (_progname, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello"
    --reshapeWindow $ windowSize 1000 600
    reshapeCallback $= Just reshape 
    displayCallback $= display
    mainLoop

reshape :: ReshapeCallback
reshape size = do 
    viewport $= (Position 0 0, size)


display :: DisplayCallback
display = do 
    clear [ColorBuffer]
    renderPrimitive Polygon $ mapM_
        (\(x,y,z) -> vertex $ Vertex3 x y z)
            myPoints
    renderPrimitive QuadStrip $ mapM_
        (\(x,y,z)->vertex $ Vertex3 x y z)
            myQuad 
    flush
