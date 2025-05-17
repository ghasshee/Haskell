module Chap1 where 


import Prelude hiding (length, head , scanl, until) 


-- 

type Nat = Int 


-- 1.1 Basic Types 

label :: [a] -> [(Nat,a)] 
label = zip [0..]  



length :: [a] -> Nat 
length = foldr (const succ) 0 


flip :: (a -> b -> c) -> b -> a -> c 
flip f x y = f y x 



-- 1.2 Lists 

head :: [a] -> a 
head = foldr const (error"List has No Element")   



concatr, concatl :: [[a]] -> [a]
concatr = foldr (++) []
concatl = foldl (++) [] 



scanl :: (b->a->b) -> b -> [a] -> [b] 
scanl f e []        = [e]
scanl f e (x:xs)    = e : scanl f (f e x) xs  




-- 1.3 Inductive and Recursive Defs 


-- perms1 leads to Insertion Sort 
perms1 :: [a] -> [[a]] 
perms1 []       = [[]] 
perms1 (x:xs)   = [ zs | ys <- perms1 xs, zs <- inserts x ys ] 

inserts :: a -> [a] -> [[a]] 
inserts x [] = [[x]]
inserts x (y:ys) = (x:y:ys) : map (y:) (inserts x ys)  


perms1' :: [a] -> [[a]] 
perms1' = foldr (concatMap . inserts)  [[]] 


-- perms2 leads to Selection Sort 
perms2 :: [a] -> [[a]] 
perms2 []       = [[]]
perms2 (x:xs)   = [ x:zs | (x,ys) <- picks xs, zs <- perms2 ys ] 

picks :: [a] -> [(a,[a])]
picks []        = []
picks (x:xs)    = (x,xs): [(y, x:ys) | (y,ys) <- picks xs ] 




until :: (a -> Bool) -> (a -> a) -> a -> a 
until p f x = if p x then x else until p f (f x)

while :: (a -> Bool) -> (a -> a) -> a -> a 
while p = until (not . p) 




-- 1.4  Fusion 

-- map f . map g        == map (f . g)
-- concatMap f . map g  == concatMap (f . g)
-- foldr f e . map g    == foldr (f . g) e 

-- Question : 
-- foldr f e . concat   == ???? 
--
-- Answer : 
-- Think Simpler Case 
-- foldr f e (xs ++ ys) == foldr f (foldr f e ys) xs
-- 
-- So, the answer is 
-- foldr f e . concat   == foldr f (flip (foldr f)) e 





-- 1.5 Accumulating and Tuples 

collapse1 :: [[Int]] -> [Int] 
collapse1 xss = help [] xss 
    where 
    help xs xss = if sum xs > 0 || null xss then xs 
                  else help (xs ++ head xss) (tail xss) 


eg1 = collapse1 [[1],[-3],[2,4]] 
eg2 = collapse1 [[-2,1],[-3],[2,4]] 
eg3 = collapse1 [[-2,1],[3],[2,4]]


labelsum xss        =   zip (map sum xss) xss 

collapse2 xss = help (0,[]) (labelsum xss) 
    where 
    help (s,xs) xss     =   if s > 0 || null xss 
                                then xs 
                                else help (cat (s,xs) (head xss)) (tail xss) 
    cat (s,xs) (t,ys)   =   (s + t, xs ++ ys) 


collapse3 xss = help (0,id) (labelsum xss) []
    where 
    help (s,f) xss      =   if s > 0 || null xss 
                                then f 
                                else help (s+t,f . (xs ++)) (tail xss) 
                                    where (t,xs) = head xss

collapse = collapse3 
