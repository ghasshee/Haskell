{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-} 


module Test where 



import Control.Lens hiding ((<.>))
import Numeric.LinearAlgebra.Static.Backprop
import Numeric.Backprop

import Numeric.OneLiner
import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bitraversable
import Data.Foldable
import Data.IDX
import Data.List
import Data.List.Split

import System.FilePath hiding ((<.>))

import GHC.Generics (Generic) 
import qualified Numeric.LinearAlgebra.Static as H


data Net    =   N   { _weights1 :: L 250 784
                    , _bias1    :: R 250
                    , _weights2 :: L 10 250
                    , _bias2    :: R 10     } 
                    deriving (Generic) 

makeLenses ''Net


logistic :: Floating a => a -> a 
logistic x = 1 / (1 + exp (-1))

softMax :: Reifies s W => BVar s (R 10) -> BVar s (R 10)
softMax x = expx / konst (norm_1V expx) where expx = exp x

crossEntropy :: (KnownNat n, Reifies s W)=> BVar s (R 10) -> BVar s (R 10) -> BVar s Double
crossEntropy targ res = - (log res <.> targ) 

runNet :: Reifies s W => BVar s Net -> BVar s (R 784) -> BVar s (R 10) 
runNet n x = z 
    where 
            y = logistic $ (n ^^. weights1) #> x + (n ^^. bias1) 
            z = softMax  $ (n ^^. weights2) #> y + (n ^^. bias2) 

netErr :: Reifies s W => BVar s (R 784) -> BVar s (R 10) -> BVar s Net -> BVar s Double
netErr x targ n = crossEntropy targ (runNet n x)

stepNet :: R 784 -> R 10 -> Net -> Net 
stepNet x targ net0 = net0 - 0.02 * gr 
    where 
            gr :: Net
            gr  = gradBP (netErr (constVar x)(constVar targ)) net0
