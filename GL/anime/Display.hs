module Display (idle, display) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Cube
import Points

type G = GLfloat
v = Vector3
c = Color3
f x = (x+1)/2

display :: IORef G -> IORef(G,G) -> DisplayCallback
display angle pos = do
  clear [ColorBuffer,DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  (x,y) <- get pos
  translate $ v x y 0
  preservingMatrix $ do
    a <- get angle
    rotate a $ v 0 0 1
    rotate a $ v 0 0.1 1        -- changed y-component a bit to show off cube corners
    scale 0.7 0.7 (0.7::G)
    forM_ (points 19) $ \(x,y,z)-> preservingMatrix $ do
      color     $ c (f x)(f y)(f z)
      translate $ v x y z
      cube 0.1
      color     $ c (0::G) 0 0      -- set outline color to black
      cubeFrame 0.1                 -- draw the outline
  swapBuffers

idle :: IORef G -> IORef G -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing
