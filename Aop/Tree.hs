

module Aop.Tree where

import GHC.Base (liftA2) 
import Data.Char

import Aop.List
import Aop.Int
import Comonad.Comonad

-------------------
--- BINARY TREE ---
-------------------

--     Bree
--
--       6
--      / \
--     3   9       
--    / \
--   1   4

data BTree a'      = Lf  | Br (a', BTree a', BTree a')

instance Show a' => Show (BTree a') where 
    show t = showRight "" t
        where 
        showLeft  str Lf          = ""
        showLeft  str (Br(a,l,r)) = str ++ "+- " ++ show a ++ "\n" ++ 
                                    showLeft (str ++ "|  ") l ++
                                    showRight (str ++ "|  ") r
        showRight str Lf          = ""
        showRight str (Br(a,l,r)) = str ++ "+- " ++ show a ++ "\n" ++ 
                                    showLeft (str ++ "   ") l ++
                                    showRight (str ++ "   ") r

lf          = Lf
br a x y    = Br(a,x,y)

fromList [] = lf
fromList (x:xs) = br x smaller larger where
    smaller         = fromList [ a | a <- xs, a <= x ]
    larger          = fromList [ a | a <- xs, a > x ] 



data Set a = BTree a 

insert a Lf                     = br a lf lf 
insert a (Br(b,x,y)) | a<b      = br b (insert a x) y
insert a (Br(b,x,y))            = br b y (insert a y)  


-- e.g.
bt = br 1 
        (br 3 
            (br 5 
                lf 
                (br 4 lf lf)) 
            (br 7 
                (br 10 
                    (br 9 
                        lf 
                        (br 3 
                            lf
                            (br 4 lf lf )))
                    (br 4 lf lf)) 
                (br 8 lf lf))) 
        ( br 4 
            lf 
            (br 9 
                (br 23 lf lf)
                (br 1 lf lf)))

--     Bree
--
--       *
--      / \
--     *   8       
--    / \
--   2   5

data Bree a' = Tip a' | Bin (Bree a', Bree a')
instance Show a' => Show (Bree a') where show t = showBree "" t 
showBree str (Tip x)     = " "   ++ show x ++ "\n"
showBree str (Bin (x,y)) = "+-"  ++ showBree (str ++ "| ") x ++ str ++ 
                           "+-"  ++ showBree (str ++ "  ") y 
tip x   = Tip x
bin x y = Bin(x,y)

foldb h c (Tip x)    = c x
foldb h c (Bin(x,y)) = h (foldb h c x) (foldb h c y)
btree f = foldb bin (tip.f)  
btree' f x y = (btree (\(x,y) -> f x y)) (combine x y)
combine (Tip x)   (Tip y)    = Tip(x,y)
combine (Bin(x,y))(Bin(a,b)) = Bin(combine x a,combine y b)

size  = foldb (+) one 
depth = foldb (curry(succ.(uncurry max))) zero 


--e.g. 
bre = bin
        (bin
            (bin
                (bin
                    (bin(tip 1)(tip 2))
                    (tip 3))
                (bin
                    (bin(tip 4)(tip 5))
                    (bin
                        (tip 6)
                        (bin(tip 7)(tip 8)))))
            (bin(tip 12)(tip 13)))
        (tip 9)



----------------
--   GTREE   ---
----------------

--     GTree                 
--
--       6           
--      /|\            
--     3 5 *           
--     | |              
--     * *             

data GTree a' = Node (a', Listl (GTree a'))
instance Show a' => Show (GTree a') where show t = show $ gtree2tree t

node a x = Node(a,x)

foldg g h d c (Node(a,Lin))        = g a d 
foldg g h d c (Node(a,Snoc(xs,x))) = g a (h (foldg g h d c x)
                                            (((foldl' h c).(listl(foldg g h d c)))xs))
gtree f = foldg (node.f) snoc lin lin
gtree2tree = foldg fork cons nil nil

gsize  = foldg (const succ) (+) 0 0 
gdepth = foldg (const succ) (max) 0 0   


--            curryg  
--          ---------> 
--   GTree              Bree
--          <---------
--           uncurryg

curryg :: GTree a -> Bree a
curryg (Node(a,Lin)) = tip a 
curryg (Node(a,x))   = bin (tip a) (branchg x) where
    branchg (Snoc(Lin,x))= curryg x
    branchg (Snoc(xs,x)) = bin (branchg xs) (curryg x)

uncurryg :: Bree a -> GTree a
uncurryg (Tip a)    = node a lin
uncurryg (Bin(Tip a,Tip b)) = node a (snoc (node b lin) lin) 
uncurryg (Bin(x,Tip b)) = unbranch x (snoc (node b lin) lin)
uncurryg (Bin(x,y)) = unbranch x (snoc (uncurryg y) lin)
unbranch (Tip a) l = node a l
unbranch (Bin(x,Tip a)) l = unbranch x (snoc (node a lin) l)
unbranch (Bin(x,y)) l = unbranch x (snoc (uncurryg y) l)



-- e.g.
chrtree         = gtree (chr.fromInteger.(+64)) gt
numtree         = gtree ord chrtree
chrtree2str ct  = foldg (++) (++) [] [] $ gtree (\c->[c]) ct  
gt = Node(1,Snoc(Snoc(Snoc(Snoc(Lin,
         Node(3,Lin)),
         Node(4,Lin)),
         Node(5,Snoc(Snoc(Lin,
             Node(8,Lin)),
             Node(9,Snoc(Snoc(Lin,
                 Node(10,Lin)),
                 Node(2,Lin)))))),
         Node(6,Snoc(Lin,
             Node(7,Lin)))))


------------------
----   TREE   ----
------------------

--  Tree                 
--       6           
--      /|\            
--     3 5 *           
--    / /|\             
--   * 4 6 *           
--     | |
--     * *
                         
data Tree a' = Fork (a', Listr (Tree a'))
instance Show a' => Show (Tree a') 
   where show t = showTree "   " t
showTree str (Fork(a,x)) = "+- " ++ show a ++ "\n" ++ showForest str x
showForest str Nil = ""
showForest str (Cons(Fork(a,x),Nil))= str ++ "+- " ++ show a ++ "\n" ++ 
                                      showForest (str ++ "   ") x 
showForest str (Cons(Fork(a,x),xs)) = str ++ "+- " ++ show a ++ "\n" ++
                                      showForest (str ++ "|  ") x ++ showForest str xs
fork a x = Fork(a,x)

foldt g h d c (Fork(a,Nil)) = g a d
foldt g h d c (Fork(a,xs))  = g a (foldf g h d c xs)
foldf g h d c  Nil          = c
foldf g h d c (Cons(x,xs))  = h (foldt g h d c x) (foldf g h d c xs)

tree2gtree = foldt node snoc lin lin 

-- e.g. 
tt = 
    Fork (1,
        Cons(Fork (1,
                Cons(Fork(4,Cons(Fork(6,Nil),Cons(Fork(7,Nil),Nil))),
                Cons(Fork(5,Nil),Nil))),
        Cons(Fork (2,Nil),
        Cons(Fork (3,Nil),Nil))))

instance Functor Listr where 
    fmap f Nil              = Nil 
    fmap f (Cons(x,xs))     = Cons(f x,fmap f xs)

instance Functor Tree where 
    fmap f (Fork(a,Nil))        = Fork(f a,Nil) 
    fmap f (Fork(a,Cons(x,xs))) = Fork(f a,Cons(fmap f x,fmap (fmap f) xs))

instance Comonad Tree where 
    extract (Fork(a,Nil))               = a 
    extract (Fork(a,Cons(x,xs)))        = a
    extend treeB2A (Fork(b,Nil))        = Fork(treeB2A (Fork(b,Nil)),Nil) 
    extend treeB2A (Fork(b,Cons(x,xs))) = Fork(treeB2A (Fork(b,Cons(x,xs))),fmap(extend treeB2A)(Cons(x,xs)))

sum_tree = foldt (+) (+) 0 0 

-- Try: 
-- >>> extend sum_tree tt 


-----------------------
-- Simple Calculator --
-----------------------


data TmArith    = TmPlus    -- nonterminal 
                | TmMult
                | TmInt Int -- terminal 
                deriving Show 


tmPlus a b = Fork(TmPlus,Cons(a,Cons(b,Nil)))
tmMult a b = Fork(TmMult,Cons(a,Cons(b,Nil)))
tmInt a    = Fork(TmInt a,Nil) 


eval1 (Fork(TmInt a,Nil))               = TmInt a 
eval1 (Fork(tm,Cons(a,Cons(b,Nil))))    = getOp tm (eval1 a) (eval1 b) 

getOp TmPlus = (+++)
getOp TmMult = (***)

(+++) (TmInt a)(TmInt b) = TmInt $ a + b
-- (+++) _ a = a 
(***) (TmInt a)(TmInt b) = TmInt $ a * b  

-- eval = foldt (+++) (+++) (TmInt 0) (TmInt 0) 


-- e.g. 

tm = tmMult
        (tmPlus
            (tmInt 4)
            (tmInt 7))
        (tmMult
            (tmPlus
                (tmInt 8)
                (tmInt 3))
            (tmInt 9))

-- Try followings: 
-- >>> tm 
-- >>> eval1 tm 
-- >>> extend eval1 tm 









-------------------------------
---   Garbage Collection    ---
-------------------------------


-- instance Show a' => Show (GTree a') 
--   where show t = showGTree "   " t
-- showGTree str (Node(a,x)) = "+- " ++ show a ++ "\n" ++ showGForest str x
-- showGForest str Lin = ""
-- showGForest str (Snoc(xs,Node(a,x))) = str ++ "+- " ++ show a ++ "\n" ++
--                                      showGForest (str ++ "|  ") x ++ showGForest str xs
