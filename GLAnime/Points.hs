module Points (points)where

import Graphics.Rendering.OpenGL

type G = GLfloat

points :: Int -> [(G,G,G)]
points n = [ (sin(2*pi*k/n'),cos(2*pi*k/n'),0) | k<-[1..n'] ]
   where n' = fromIntegral n
