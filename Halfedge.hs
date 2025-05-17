
module Halfedge where 

import Data.IORef

data Halfedge   = Halfedge  { twin :: IORef Halfedge
                            , next :: IORef Halfedge 
                            }  

{--
data HalfGraph a    = Empty 
                    | Vertex a 
                    | Overlay (Graph a) (Graph a)
                    | HalfConnect (Graph a) (Graph a) 
--} 

data Graph a    = Empty 
                | Vertex a 
                | Overlay (Graph a) (Graph a) 
                | Connect (Graph a) (Graph a)
                deriving Show 


instance Num a => Num (Graph a) where 
    fromInteger     = Vertex . fromInteger 
    (+)             = Overlay 
    (*)             = Connect
    signum          = const Empty 
    abs             = id 
    negate          = id 

{--
instance Ord a => Eq (Graph a) where 
    (==) = eqR 

eqR :: Ord a => Graph a -> Graph a -> Bool
eqR x y = toAdjacencyMap x == toAdjacencyMap y
--} 

instance Functor Graph where
    fmap f g = g >>= (vertex . f) 
    {-# INLINE fmap #-} 

instance Applicative Graph where
    pure    = Vertex
    f <*> x = buildg $ \e v o c -> foldg e (\w->foldg e(v.w)o c x) o c f 
    {-# INLINE (<*>) #-} 

instance Monad Graph where 
    g >>= f = buildg $ \e v o c -> foldg e (foldg e v o c . f) o c g
--    {-# INLINE #-} 
    
vertex      ::  a -> Graph a 
vertex      =   Vertex
{-# INLINE vertex #-} 

edge        ::  a -> a -> Graph a 
edge x y    =   Connect(vertex x)(vertex y) 
{-# INLINE edge #-}

overlay     ::  Graph a -> Graph a -> Graph a
overlay     =   Overlay
{-# INLINE overlay #-} 

foldg e v o c  Empty        = e 
foldg e v o c (Vertex x)    = v x 
foldg e v o c (Overlay x y) = o (foldg e v o c x)(foldg e v o c y)
foldg e v o c (Connect x y) = c (foldg e v o c x)(foldg e v o c y)
{-# INLINE [0] foldg #-} 


buildg f = f Empty Vertex Overlay Connect
{-# INLINE [1] buildg #-} 

composeR = (.) 




v :: Integer -> Graph Integer
v n = fromInteger n 


