module Control.Alternative.hs where


class Alternative 
    some                ::  f a -> f [a]
    some v              =   some_v where 
        many_v              =   some_v <|> pure []
        some_v              =   (:) <$> v <*> many_v 
    many                ::  f a -> f [a]
    many v              =   many_v where 
        many_v              =   some_v <|> pure []
        some_v              =   (:) <$> v <*> many_v  
