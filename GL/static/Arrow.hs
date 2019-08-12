import RenderArrow as RA
import Graphics.UI.GLUT
import Complex 

camera = [0,0,1]
target = [0,0,0]
up     = [0,1,0]

z_z2_z3 z = (z - z*z - z*z*z) / (C 5 0)
fish z = (C (-1) 0) + (C 0 (-1)) * (z - (C 0.5 (-0.5))) * (z - (C (-1) 0))  
z2_1 z = (z - (C 1 0))*(z + (C 1 0) )
eg01 z = (z - (C 0 1)) * (z - (C 1 0))

f01 z = acos z
f02 z = z 
f03 z = cos z  
f04 z = exp z 
f05 z =  (z -(C 0 1)) * (z -(C 0.5 0.8)  )
f06 z = z^2 + z
f07 z = - ( z - (C 0 1) ) / (C 2 0) 
__f z = z^2 + 1
_f  = (zoomdown 1). __f  -- .reciprocal   


zoomdown n z = z / (C n 0)
zoomup   n z = z * (C n 0) 

f  z = (c2a. _f .a2c) z 

g [x,y,z] = [x,y,z]
gradient2d          = id
hamilton2d          = normal2d
drawField field f [x,y,z] = [x,y,z] - (mul 0.2 (field (f [x,y,z]))) 

vec [x,y,z] = Vector3 x y z 

main :: IO ()
main = do 
    (_prog, _args)      <- getArgsAndInitialize
    initialWindowSize   $= Size 800 800
    _window             <- createWindow "arrow"
    displayCallback     $= display
    mainLoop

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    -- lookAt (p2v $ map gl camera) (p2v $ map gl target) (vec $ map gl up)
    displayImage f
    -- displayField hamilton2d g
    -- displayField gradient2d g 
    flush

p2l          field f p  = [drawField field f p, p]
displayField field f    = mapM_ ( renderArrow . p2l field f ) ggp 
displayImage       f    = mapM_ ( renderLine (c 1 0 0) ) ( action f (grid 1 0.05 0.005))

v           = Vertex3 
c r g b     = Color3 r g (gl b) 

ggp        :: [[GLdouble]]
ggp        = [ map gl [x,y,0] | x <- [-1,-0.95..1], y <- [-1,-0.95..1] ]

action f grid = fmap (fmap (riemannize' . f)) grid 


translate' :: [GLdouble] -> [GLdouble] -> [GLdouble] 
translate' [x,y,z] [a,b,c]      = [a,b,c] + [x,y,z] 

points2lines starts ends = fmap anzip (zip starts ends)
    where anzip (s,t) = [s,t]

grid n interval epsilon     = xlines n interval epsilon ++ ylines n interval epsilon 

xlines :: GLdouble -> GLdouble -> GLdouble -> [[[GLdouble]]]
xlines n interval epsilon   = 
    points2lines xlines_points (map (translate' [0.005,0,0]) xlines_points) where 
        xlines_points = [[x,y,0] | x <- [-n, -n+epsilon .. n], y <- [-n, -n+interval .. n]]
ylines :: GLdouble -> GLdouble -> GLdouble -> [[[GLdouble]]]
ylines n interval epsilon   = 
    points2lines ylines_points (map (translate' [0,0.005, 0]) ylines_points) where 
        ylines_points = [[y,x,0] | x <- [-n, -n+epsilon .. n], y <- [-n, -n+interval .. n]]




{--
 - lines_grid f      =
    map anzip (zip (map rf start) (map rf end))
        where
            start = grid 3 0.05 0.005 0 
            end   = map (transpose [
            rf = riemannize' . f 
            anzip (s,t) = [s,t] 

grid n interval epsilon =
    [ map gl [x,y,0] | x <- [-n+epsilon,-n+epsilon+epsilon .. n+epsilon], y <- [-n,-n+interval .. n]] ++
    [ map gl [y,x,0] | x <- [-n+epsilon,-n+epsilon+epsilon .. n+epsilon], y <- [-n,-n+interval .. n]] --}

