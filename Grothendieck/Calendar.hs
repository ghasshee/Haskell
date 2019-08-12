import Data.Set 

data Section k a    = Section { ivmap :: IntervalMap k a }
type Domain k       = Section k ()

class EqOn a d where 
    eqOn :: d -> a -> a -> Bool


restrict :: (Ord k) 
    => Section k a -> Domain k -> Section k a

glue :: (Ord k, EqOn Domain a)
    => Section k a -> Section k a -> Maybe (Section k a)


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
            |Dec deriving (Enum, Ord, Show)

c   = [Jan .. Dec]
c0  = toList $ fromList c \\ fromList [Dec]
c1  = toList $ fromList c \\ fromList [Jun]
c01 = [Jan .. Jun]
c10 = [Jul .. Dec]
zero= []



