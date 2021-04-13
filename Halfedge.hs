
module Halfedge where 

import Data.IORef

data Halfedge   = Halfedge  { twin :: IORef Halfedge
                            , next :: IORef Halfedge 
                            }  


data Graph a    = Empty 
                | Vertex a 
                | Overlay (Graph a) (Graph a) 
                | Connect (Graph a) (Graph a)
                deriving Show 
{--
data HalfGraph a    = Empty 
                    | Vertex a 
                    | Overlay (Graph a) (Graph a)
                    | HalfConnect (Graph a) (Graph a) 
--} 


