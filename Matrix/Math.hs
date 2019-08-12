module Math where


-- List is Num with following definitions:
instance (Num a) => Num [a] where 
    a      + []     = a
    []     + a      = a
    (x:xs) + (y:ys) = (x+y):(xs+ys)
    a      - []     = a
    []     - a      = negate a
    (x:xs) - (y:ys) = (x-y):(xs-ys)
    a      * []     = []
    []     * a      = []
    (x:xs) * (y:ys) = (x*y):(xs*ys)
    negate []       = []
    negate (x:xs)   = (negate x):(negate xs)
    signum []       = []
    signum (x:xs)   = (signum x):(signum xs)
    fromInteger n   = [fromInteger n]
    abs    []       = []
    abs    (x:xs)   = (abs x):(abs xs)


delta i j = if i == j then 1 else 0

---quicksort 
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) =
    let lowerequal = [ l | l<-xs, l<=x ]
        greater    = [ g | g<-xs, g>x  ]
    in sort lowerequal ++ [x] ++ sort greater 

