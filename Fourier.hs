import Data.Complex 

fourierTransform    :: RealFloat a => [Complex a] -> [Complex a] 
fourierTransform xs = flip map [0..n-1] (\i -> foldr (\(j,aj) -> (+) (f aj i j)) 0 (zip [0..] xs)) 
    where 
        n = length xs
        f aj i j = aj * cis( 2*pi*fromIntegral (i*j) /fromIntegral n ) 
