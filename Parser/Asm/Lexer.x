{ 
    module Lexer (Token(..), P, evalP, lexer) where 
    import Control.Monad.State
    import Control.Monad.Error
    import Data.Word

    import Syntax
} 

$digit = [0-9]
$space = [\  \t]
$init  = [a-z A-Z \_ \: \. \$]
@var   = $init [$init $digit]*


-- tokens :- 
--    '@' var     {A_ (V_ var) } 


tokens :- 
    $white+     ;
    true        {TTrue} 
    false       {TFalse}
    ∀           {TZero 0 }
    succ        {\s -> succ }
    pred        {TPred}
    if          {TIf}
    then        {TThen}
    else        {TElse}
    iszero      {TIsZero}

{

    type AlexInput = [Word8] 
    alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
    alexGetByte (b:bs)  = Just (b,bs)
    alexGetByte []      = Nothing

    alexInputPrevChar :: AlexInput -> Char
    alexInputPrevChar   = undefined 

    type P a    = StateT AlexInput (Either String) a 
    
    evalP :: P a -> AlexInput -> Either String a 
    evalP       = evalStateT

    readToken :: P Token
    readToken = do 
        s <- get 
        case alexScan s 0 of 
            AlexEOF                 -> return TEOF
            AlexError _             -> throwError "!Lexical Error" 
            AlexSkip input _        -> do { put input; readToken } 
            AlexToken input _ tk    -> do { put input; return tk }

    lexer :: (Token -> P a) -> P a
    lexer cont = readToken >>= cont

}
