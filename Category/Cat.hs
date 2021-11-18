{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE PolyKinds #-} 
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BlockArguments #-} 
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneKindSignatures #-} 

import Data.Kind (Constraint, Type) 

type (~>) :: forall k. (k-> Type) -> (k -> Type) -> Type 
type f ~> g = (forall x. f x -> g x) 



