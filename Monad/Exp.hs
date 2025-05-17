module Exp where 

import Kan 
import Data.Functor.Identity 

type Exp a b = Lan ((,) a) Identity b 



