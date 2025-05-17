module Matrix.Vector where 

import Matrix.Matrix

data Vector a  = Vec [a]

instance Num a => Num (Vec a) where 
    a + b = a 
    a * b = a

