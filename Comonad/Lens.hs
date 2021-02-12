{-# LANGUAGE RankNTypes #-} 


class Functor w => Commonad w where 
    extract     :: w a -> a 
    duplicate   :: w a -> w (w a)



data Store b a = Store (b->a) b   


instance Functor (Store b) where 
    fmap f (Store v b) = Store (f . v) b 


instance Commonad (Store b) where 
    extract     (Store v b)     = v b 
    duplicate   (Store v b)     = Store (Store v) b



-- CoAlgebra 

-- Simple Lens


type Lens' a b = a -> Store b a

lens'           :: (a->b) -> (a->b->a) -> Lens' a b 
lens' get set   = \a -> Store (\b -> set a b) (get a) 

get'            :: Lens' a b -> a -> b  
get' l a        = let Store _ b = l a in b

set'            :: Lens' a b -> a -> (b->a)
set' l a        = let Store v _ = l a in v 


-- Example

_fst'   ::  Lens' (a,b) a 
_fst'   =   lens' fst setFst
    where setFst (_,x) y = (y,x)

_snd'   ::  Lens' (a,b) b
_snd'   =   lens' snd setSnd 
    where setSnd (x,_) y = (x,y) 


-- ghci> get' _fst' (1,2) 
-- 1 

{--
type Lens'' s a  = forall r. Lens' r a -> Lens' r s 

lens'' :: (s->a) -> (s->a->s) -> Lens'' s a 
lens'' get set f s = let Store v r = f(get s) in Store (\_ -> set s(v r))r
--}


type Lens s t a b = forall r. (a->Store r b) -> (s->Store r t) 

lens :: (s->a) -> (s->b->t) -> Lens s t a b
lens get set f = \s -> let Store v r = f(get s) in Store (\_ -> set s(v r))r
-- get s    :: a
-- set s    :: b -> t 
-- f        :: a -> Store r b    (the type of OLD lens) 
-- return   :: s -> Store r t    (the type of NEW lens)
--
-- 'forall r. Store r'  means the Universal Store !


get :: Lens s s a a -> s -> a 
get f s = let Store _ a = f (\a -> Store undefined a) s in a 

set :: Lens s t a b -> s -> (b->t) 
set f s = \b -> let Store v r = f(Store(const b)) s in v r 


-- Example

_fst :: Lens (a,x) (b,x) a b 
_fst = lens fst setFst 
        where setFst (_,x) y = (y,x) 

_snd :: Lens (x,a) (x,b) a b
_snd = lens snd setSnd 
        where setSnd (x,_) y = (x,y)


-- Usage
-- ghci> get _fst (1,2)
-- 1 
--

