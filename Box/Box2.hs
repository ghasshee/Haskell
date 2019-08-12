

data Box a = Num a | Box [[Box a]]

instance (Show a)=> Show (Box a) where 
    show = showBox


showBox (Num a) = show a
showBox x = showTop (top x) ++ showRest (rest x)

top (Num x) = Num x 
top (Box ((x:xs))
