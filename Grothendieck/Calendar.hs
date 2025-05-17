{-# LANGUAGE FlexibleInstances  #-} 
{-# LANGUAGE FlexibleInstances  #-} 
{-# LANGUAGE TypeApplications   #-} 
{-# LANGUAGE TypeFamilies       #-} 
{-# LANGUAGE DataKinds          #-} 
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE PolyKinds          #-} 
{-# LANGUAGE GADTs              #-} 
{-# LANGUAGE RankNTypes         #-} 

import Data.Set ( Set(..) )  
import Data.IntervalMap.Generic.Strict (Interval(..), IntervalMap, fromList, toList, (\\) )


data Section k a    = Section { ivmap :: IntervalMap k a }
type Domain k       = Section k ()

class EqOn a d where 
    eqOn :: d -> a -> a -> Bool


restrict :: (Ord k) 
    => Section k a -> Domain k -> Section k a
restrict = undefined 

glue :: (Ord k, EqOn (Domain k) a)
    => Section k a -> Section k a -> Maybe (Section k a)
glue = undefined 

data Month = Jan
            |Feb
            |Mar
            |Apr
            |May
            |Jun
            |Jul
            |Aug
            |Sep
            |Oct
            |Nov
            |Dec deriving (Enum, Eq, Ord, Show)

instance Interval Month () where 
    lowerBound m = ()
    upperBound m = ()


k x = (x,())
f   = fromList
t   = toList


c   = f $ map k [Jan .. Dec]
c0  = c \\ (f $ map k [Dec])
c1  = c \\ (f $ map k [Jun])
c01 = f $ map k [Jan .. Jun]
c10 = f $ map k [Jul .. Dec]
zero= f $ []


 






{--
 -
 -  Sheaf(C) = Hom_Cat(C^op, Set) 
 -
 - Actually, Implementaion of sheaf needs a lot of things
 - * Equalizer ( which is not possible to implement in Haskell
 - * type == term ( dependent type system is much better ) 
 -
 -
 - Here shows Abstract Code of a sheaf 
 -
 -
 - F(x)     | x == c    = id[Void]
 -          | otherwise = id[Int] 
 -
 - F(c0 -> c )          = const 0 
 - F([] -> x )          = const 0 
 - F(c01-> c1)          = id 
 - F(c10-> c1)          = succ
 - F(c10-> c0)          = id 
 - F(c10-> c1)          = id
 -
 -
 -
 -
 -  Category C    -->    Set  
 -                 
 -
 -     c       ^       |      Void 
 -   /   \     |       |    /      \
 -  c0   c1    |       |  Int      Int 
 -  |  X  |    |       | id| +1 X   |id
 -  c01  c10   |       |  Int      Int 
 -   \   /     |       |    \      /
 -     0       |       v       Int 
 -
 -
 --}
