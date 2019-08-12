{-# LANGUAGE StandaloneDeriving #-} 
import Data.IntervalMap.Generic.Strict 

instance Ord e => Interval (e,e) e where 
    lowerBound (a,_) = a
    upperBound (_,b) = b
    rightClosed _ = True

type MyMap = IntervalMap (Int,Int) String

sample      :: MyMap 
sample      = fromList [((1,10),"One2Ten"),((5,20),"Five2Twenty"),((15,30),"Fifteen2Thirty")]

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Enum,Eq,Ord,Show) 

type Calendar = IntervalMap (Month,Month) Int
cals        :: Calendar
cals        = fromList [((Jan,Nov),2018), ((Jul,Jun),2018), ((Feb,Dec),2019) ]

type Months     = IntervalMap (Month,Month) ()
months      :: Months
months      = fromList [((Jan,Apr),())]

data Section k a    = Section {ivmap :: IntervalMap k a} deriving Show
type Domain k       = Section k () 

class EqOn a d where 
    eqOn :: d -> a -> a -> Bool

restrict :: (Ord k) => 
    Section k a -> Domain k -> Section k a 
restrict s k = s 
