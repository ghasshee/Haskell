module Matrix.Matrix where
-- TODO : Matrix can be monad 

data Mat a = Mat [[a]]

showRow []      = ""
showRow (x:xs)  = show x ++ "\t\b" ++ showRow xs
instance (Show a) => Show (Mat a) where 
    show (Mat [])       = ""
    show (Mat (x:xs))   = showRow x ++ "\n" ++ show (Mat xs) 

size (Mat x) = (lrow x,lcol x)
lrow = (.)(.)(.) maximum map length
lcol = length 
instance (Num a) => Num (Mat a) where
    (Mat a) + (Mat b)   =
        let m = max(lrow a)(lrow b) in
        let n = max(lcol a)(lcol b) in
        Mat [[ (a!!i!!j) + (b!!i!!j) | j <- [0..(n-1)]] 
                | i <- [0..(m-1)] ]
    (Mat a) - (Mat b)   =
        let m = max(lrow a)(lrow b) in
        let n = max(lcol a)(lcol b) in
        Mat [[ (a!!i!!j) - (b!!i!!j) | j <- [0..(n-1)]] 
                | i <- [0..(m-1)] ]
    (Mat a) * (Mat b)   =
        let l = lrow a - 1 in
        let m = lcol a - 1 in
        let n = lcol b - 1 in
        Mat [[ sum [((a!!i!!k)*(b!!k!!j)) | k<-[0..m]] | j<-[0..n] ] 
                | i<-[0..l] ]
    abs (Mat a)         = det (Mat a)
    fromInteger x       = Mat [[fromInteger x]]
    signum (Mat x)      = 1

det (Mat a) = 1

transpose (Mat a) = 
        let m = lrow a in
        let n = lcol a in
        Mat [
                [ (a!!j!!i) | j <- [0..(n-1)]] 
                | i <- [0..(m-1)]
            ]



