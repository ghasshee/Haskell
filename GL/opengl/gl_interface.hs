import Graphics.UI.GLUT

type G = GLfloat
render = renderPrimitive

p :: G -> G -> G -> (G,G,G)
p = (\x y z -> (x,y,z))

line :: (G,G,G) -> (G,G,G) -> [(G,G,G)]
line = ( \pa pb -> [pa, pb] )

arrowTriangle :: [(G,G,G)] -> [(G,G,G)]
arrowTriangle = ( \[(x1,y1,z1),(x2,y2,z2)] 
                -> [((7/8*(x2-x1)+1/16*(y1-y2)+x1),(7/8*(y2-y1)+1/16*(x2-x1)+y1),z2),
                    ((7/8*(x2-x1)+1/16*(y2-y1)+x1),(1/16*(x1-x2)+7/8*(y2-y1)+y1),z2),
                    (x2,y2,z2)])

transform = (\(x,y,z) -> vertex $ Vertex3 x y z)
points3f = mapM_ transform

renderArrow line = do
    render Lines $ points3f $ line
    render Triangles $ points3f $ arrowTriangle $ line

sub = (\(u,v,w) (x,y,z) ->((x-u),(y-v),(z-w)))
mul = (\k       (x,y,z) ->((x*k),(y*k),(z*k)))


main :: IO ()
main = do 
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "arrow"
    displayCallback $= display
    mainLoop

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    
--  renderArrow $ l (p 0 0 0) (p 1 -0.5 0)      // Example arrow

    let df = (\(x,y,z)->(y,(-x),0))
    let arrow (x,y,z) = sub ( mul 0.2 $ df(x,y,z))( x,y,z )
    mapM_ (\(x,y,z) -> renderArrow $ line (arrow(x,y,z))(x,y,z)) 
            [(x,y,0)| x <-[-1,-0.75..1], y<-[-1,-0.75..1]]

    flush





