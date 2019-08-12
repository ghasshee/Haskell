module Math.Base where 

import Data.Ratio



-- Max elememt of Array
maximum' :: Ord a => [a] -> a
maximum' []     = error "null"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

{-- nth element of Array
xs     !! n|n<0 = error "negative"
[]     !! _     = error "index too large"
(x:_ ) !! 0     = x
(_:xs) !! n     = xs !! (n-1) --}
ee n x = x !! n

-- An array excluding nth element
xs     !!! n|n<0 = error "negative"
[]     !!! _     = error "index too large"
(x:xs) !!! 0     = xs
(x:xs) !!! n     = x:(xs !!! (n-1))
nee n x = x !!! n       -- Prefix ver. of (!!!)

-- GCD / LCM 
gcd' n 0 = n
gcd' n 1 = 1 
gcd' n m = if (mod n m) == 0 
                then m 
                else gcd' m (mod n m) 
lcm' n m = div (n*m) (gcd' n m)




