module GL.Test where

import Graphics.UI.GLUT

display :: IO ()
display = do
    clear [ ColorBuffer ]
    flush 

main :: IO ()
main = do
    (_prog,_args)   <- getArgsAndInitialize
    _window         <- createWindow "Hello World"
    displayCallback $= display
    mainLoop
