{-# LANGUAGE 
    KindSignatures, 
    TypeFamilies, 
    ConstraintKinds,
    PolyKinds,
    MultiParamTypeClasses #-}

module Control.Effect where 

import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )

class Effect ( m :: k -> * -> * ) where
    type Unit m :: k
    type Plus m (f :: k) (g :: k) :: k
    type Inv  m (f :: k) (g :: k) :: Constraint
    type Inv  m f g = ()

    return      ::  a -> m (Unit m) a 
    (>>=)       ::  (Inv m f g) => m f a -> (a->m g b) -> m(Plus m f g)b
    (>>)        ::  (Inv m f g) => m f a -> m g b -> m(Plus m f g)b
    x >> y      =   x >>= (\_ -> y)

fail = undefined

class Subeffect ( m :: k -> * -> * ) f g where
    sub         ::  m f a  -> m g a

