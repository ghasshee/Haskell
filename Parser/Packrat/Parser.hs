

module Parser where 

{--
 - Additive     <- Multitive '+' Additive  | Multitive 
 - Multitive    <- Primary   '*' Multitive | Primary 
 - Primary      <- '{' Additive '}'        | Decimal 
 - Dicimal      <- '0' | ... | '9'
 - --} 


data Result' v  = Parsed' v String 
                | NoParse' 


pAdditive'      :: String -> Result' Int 
pAdditive' s    =   alt1 where 
    alt1        =   case pMultitive' s of 
                        Parsed' vleft s' -> case s' of 
                            ('+':xs')       -> case pAdditive' xs' of 
                                Parsed' vright xxs' -> Parsed'(vleft + vright) xxs'
                                _                   -> alt2 
                            _               -> alt2 
                        _               -> alt2 
    alt2        =   case pMultitive' s of 
                        Parsed' v s'    -> Parsed' v s' 
                        NoParse'        -> NoParse'


pMultitive' :: String -> Result' Int 
pMultitive' = undefined 
{--
pPrimary    :: String -> Result Int 
pDecimal    :: String -> Result Int 
--} 





------------------------------------------------------
---                Packrat Parsing                 ---
------------------------------------------------------

{--
 - Additive     <- Multitive '+' Additive  | Multitive 
 - Multitive    <- Primary   '*' Multitive | Primary 
 - Primary      <- '{' Additive '}'        | Decimal 
 - Dicimal      <- '0' | ... | '9'
 - --} 

data Derivs     =   Derivs { 
                        dvAdditive  :: Result Int, 
                        dvMultitive :: Result Int,
                        dvPrimitive :: Result Int, 
                        dvDecimal   :: Result Int
                            } deriving (Show) 

data Result v   =   Parsed v Derivs 
                |   NoParse
                            deriving Show 


pAdditive       ::  Derivs  -> Result Int 
pAdditive    d  =   alt1 where 
    alt1            =   case dvMultitive d of 
        Parsed vleft d' ->  case dvChar d' of 
            Parsed '+' d''  ->  case dvAdditive d'' of 
                Parsed vright d'''  -> Parsed (vleft + vright) d'''
                _                   -> alt2 
            _               ->  alt2
        _               -> alt2 
    alt2            =   dvMultitive d 


pMultitive   d  =   alt1 where 
    alt1            =   case dvPrimaty d of 
        Parsed vleft d' ->  case dvChar d' of 
            Parsed '*' d''  ->  case dvMultitive d'' of 
                Parsed vright d'''  -> Parsed (vleft * vright) d'''
                _                   -> alt2
            _               ->  alt2 
        _               ->  alt2
    alt2            =   dvPrimary d 





