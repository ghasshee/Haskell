module Cube (vertex3f, cube, cubeFrame)where

import Graphics.UI.GLUT

type G = GLfloat
v = Vertex3

vertex3f :: (G,G,G) -> IO ()
vertex3f (x,y,z) = vertex $ v x y z


face (x,y,z) = [(x+y+z,y+x+z,z+x+y), 
                (x+y+z,y+x-z,z-x-y), 
                (x-y-z,y-x-z,z-x-y), 
                (x-y-z,y-x+z,z+x+y)]

t w = face ( 0, 0, w)
u w = face ( 0, 0,-w)
r w = face ( w, 0, 0)
l w = face (-w, 0, 0)
f w = face (0 , w, 0)
b w = face (0 ,-w, 0)

cube :: G -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f $ t w ++ u w ++ r w ++ l w ++ f w ++ b w

cubeFrame :: G -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ vertex3f $ t w ++ u w ++ r w ++ l w ++ f w ++ b w
