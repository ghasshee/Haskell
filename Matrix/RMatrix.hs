data Mat = Mat [[R]]

data R = S Int | W Mat 
showR (S x) = show x
showR (W x) = show x
instance Show R where show r = showR r

-- [xxx  ] xs     
-- [xxs  ]       
-- [     ]

top [] = []
top ((W (Mat(xxx:xxs))):xs) = (W (Mat[xxx])):(top xs)
rest [] = []
rest ((W (Mat(xxx:xxs))):xs) = (W (Mat xxs)):(rest xs)

showRow [] = ""
showRow ((S x):xs) = showRowS ((S x):xs)
showRow ((W x):xs) = showRowW ((W x):xs)
showRowS [] = ""
showRowS ((S x):xs) = show x ++ "\t" ++ showRowS xs 
showRowW ((W (Mat [])):xs) = ""
showRowW x = showTop (top x) ++ "\n" ++ showRest (rest x)
showRest ((W (Mat [xxx])):xs) = showTop ((W (Mat[xxx])):xs)
showRest ((W (Mat xxs)):xs) = showRowW ((W (Mat xxs)):xs)
showTop [] = "" 
showTop ((W (Mat[xxxx:xxxs])):xs) = show xxxx ++ "\t" ++showRow xxxs ++ showTop xs 
showMat (Mat []) = ""
showMat (Mat (x:xs)) = showRow x ++ "\n" ++ showMat (Mat xs) 
instance Show Mat where show m = showMat m



z = S 0
s = S 1
m = S (-1)

e = Mat [
    [s,z],
    [z,s]]

i = Mat [
    [z,m],
    [s,z]]

mi =Mat [
    [z, s],
    [m, z]]

n = Mat [
    [z,z],
    [z,z]]

ee = Mat [
    [W e,W n],
    [W n,W e]]

ii = Mat [
    [W i,W n],
    [W n,W mi]]


