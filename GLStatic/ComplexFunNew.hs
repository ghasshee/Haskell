{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-} 
import Graphics.UI.GLUT
import Complex_
import Three

camera  :: Three 
camera  = (0,0,1)
target  :: Three
target  = (0,0,0)
up      :: Three
up      = (0,1,0)


{-------------- FUNCTION -----------} 
z_z2_z3 z   = (z-z^2-z^3)/(5,0) :: Complex 
fish z      = -one+i*(z-(0.5,-0.5))*(z-one)  
z2_1 z      = (z-one)*(z +one)
f02 z       = (z-i)*(z-one)
f04 z       = exp z 
f05 z       = (z-i)*(z-(0.5,0.8))
f06 z       = z^2 + z
f07 z       = -(z-i)/two :: Complex 
f08 z       = pow i (z^2) 
-- strip  (x,y)  =  pow 3 (x,y+3) +  pow 2 (x,-y+3)  
--  f  z        = pow (1,0.2)   
--  f  z        = z * pow i z 
--  f  z        =  pow i  z  
--  f  z        = (-)(0.2,0.3) $ pow(1,0.3) $ z^2+one+z 
--  f  z        =  pow (sqrt z) z  
-- joukowski z = z + one/z 
-- f z = sin (pow z i)  

f           :: Complex -> Complex 
--f  = id  
-- acos+ z =   i * log ( z + (pow (0.5,0) (z^2 + 1)))
-- acos- z = - i * log ( z + (pow (0.5,0) (z^2 + 1)))

f z = z^3 - (3,0) * z

--f         (x,y) = exp (pow (1,0.05) ((x,y)+6*i) ) 
f' f         = modify f id 5

{------------------- MAIN -------------------} 

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    -- lookAt (toVtx camera) (toVtx target) (toVec up)
    joukowskiAirfoil
    -- displayGrid 1 1 
    flush

main                            ::  IO ()
main                            =   do 
    (_prog, _args)                  <- getArgsAndInitialize
    initialWindowSize               $= Size 800 800
    _window                         <- createWindow "arrow"
    displayCallback                 $= display
    mainLoop

{----------------- FILTER -------------------}

modify f riemann n              =   filter sieve . map ( map ( riemann . fromComplex . zoomdown n . f . toComplex )) 
zoomdown n z                    =   z/(n,0) :: Complex 

sieve                           ::  [Three] -> Bool
sieve [(x,y,z),(a,b,c)]         =   (x-a)^2+(y-b)^2+(z-c)^2 <= 0.04 

riemann                         =   riemannize . toComplex
riemannize (x,y)                =   (2*x/(1+r2), 2*y/(1+r2),(r2-1)/(r2+1)) where r2 = x^2+y^2 

toGL                            ::  Double -> GLdouble
toGL                            =   id
fromGL                          ::  GLdouble -> Double 
fromGL                          =   id 

{------------------ DISPLAY -----------------} 

displayImage r g b n []         =   do  return ()
displayImage r g b n (ln:lns)   =   do  renderLine (Color3 r g (toGL b)) ln
                                        displayImage r g b n lns 
    
renderLine c line               =   do
    renderPrimitive Lines $ do color c ; mapM_ vertex $ map toVtx line

{-------------- CIRCLE DISPLAY --------------}

joukowskiAirfoil                =   do 
    circle2airfoil 20 20

displayCircle                   =   do 
    displayImage 1 0 0 0 $ f' f     $ circle (-0.11,0.11)   0.11 10000
    displayImage 0 1 1 0 $ f' id    $ circle (-0.1,0.1)     0.1 1000

circle2airfoil n 0              =   return () 
circle2airfoil n k              =   do
    displayImage (1-k/n)(k/n)(k/n) 0 $ f' (\z->z+(1-k/n,0)/z) $ circle (-0.1,0.1) 0.1 ε
    circle2airfoil n(k-1)

circle (a,b) r ε                =   pts2lines(map fromComplex circle_pts)(map(fromComplex . ((*)(exp(0,2*pi*ε))))circle_pts)  
    where circle_pts            =   [ (a,b) + exp (r, 2*k*pi) | k<-[0,ε..1] ]  :: [Complex]

{-------------- GRID DISPLAY --------------} 
ε = 1/400
δ = 1/20

displayGrid x y = do
    displayImage 1 0.7 1 0      $ f' f $ grid (δ,-δ) (x,δ)     δ ε
    displayImage 1 0.5 0.5 0    $ f' f $ grid (-δ,δ) (δ,y)     δ ε
    displayImage 0.5 1 0.5 0    $ f' f $ grid (-x,-δ)(-δ,δ)    δ ε
    displayImage 0.7 1 1 0      $ f' f $ grid (-δ,-y)(δ,-δ)    δ ε
    displayImage 1 0 1 0        $ f' f $ grid (δ,δ)  (x,y)     δ ε 
    displayImage 1 1 0 0        $ f' f $ grid (-x,δ) (-δ,y)    δ ε 
    displayImage 0 1 1 0        $ f' f $ grid (-x,-y)(-δ,-δ)   δ ε 
    displayImage 1 1 1 0        $ f' f $ grid (δ,-y) (x,-δ)    δ ε 

grid                            ::  Complex -> Complex  -> GLdouble -> GLdouble -> [[Three]]
grid   a b itval ε              =   xlines a b itval ε ++ ylines a b itval ε 
xlines                          ::  Complex -> Complex -> GLdouble -> GLdouble -> [[Three]]
xlines (a,b)(c,d) itval ε       =   pts2lines xlines_pts (fmap (translate' (ε,0,0)) xlines_pts) 
    where   xlines_pts          =   [(x,y,0) | x <- [a, a+ε .. c], y <- [b, b+itval .. d]]
ylines                          ::  Complex -> Complex -> GLdouble -> GLdouble -> [[Three]]
ylines (a,b)(c,d) itval ε       =   pts2lines ylines_pts (fmap(translate' (0,ε,0))ylines_pts) 
    where   ylines_pts          =   [(x,y,0) | x <- [a, a+itval .. c], y <-[b, b+ε .. d]] :: [Three] 
pts2lines starts ends           =   fmap anzip $ zip starts ends
    where  anzip (p,q)          =  [p,q] 
translate'                      ::  Three  -> Three -> Three
translate' (x,y,z) (a,b,c)      =   (a,b,c) + (x,y,z) 


