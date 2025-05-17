import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU
import Animation


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

