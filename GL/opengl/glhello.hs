import Graphics.UI.GLUT
 
main :: IO ()
main = do 
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello World"
    displayCallback $= display
    mainLoop

display :: DisplayCallback
display = do
    clear [ ColorBuffer ]
    flush
    
    height <- new IORef 1.0
    currentHeight <- get height
    height $= 1.5


