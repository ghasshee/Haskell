module Matrix.MonadMatrix where



data Mat n a = Mat n [[a]]



instance Functor (Mat n) where
    fmap f (Mat n x) = Mat n (fmap' f (x)) where 
        fmap' f [] = []
        fmap' f (x:xs) = (fmap f x):(fmap' f xs)    

instance Applicative (Mat n) where
    pure a = Mat n []






showRow []      = ""
showRow (x:xs)  = show x ++ "\t\b" ++ showRow xs
instance (Show a) => Show (Mat n a) where 
    show (Mat n [])       = ""
    show (Mat n (x:xs))   = showRow x ++ "\n" ++ show (Mat n xs) 

size (Mat n x) = n 

instance (Num a) => Num (Mat n a) where
    (Mat n a) + (Mat m b) | m==n  =
        Mat n [[ (a!!i!!j) + (b!!i!!j) | j <- [0..(n-1)]] 
                | i <- [0..(n-1)] ]
    (Mat n a) - (Mat m b) | m==n  =
        Mat [[ (a!!i!!j) - (b!!i!!j) | j <- [0..(n-1)]] 
                | i <- [0..(n-1)] ]
    (Mat n a) * (Mat m b) | m==n  =
        Mat [[ sum [((a!!i!!k)*(b!!k!!j)) | k<-[0..n]] | j<-[0..n] ] 
                | i<-[0..n] ]
    abs (Mat n a)         = det (Mat n a)
    fromInteger x       = Mat n [[fromInteger x]]
    signum (Mat n x)      = 1

det (Mat n a) = 1

transpose (Mat n a) = 
        Mat n [
                [ (a!!j!!i) | j <- [0..(n-1)]] 
                | i <- [0..(n-1)]
            ]



