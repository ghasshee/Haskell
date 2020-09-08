
-- Backus-Naur Form 
-- number   = [ "-" ] digit { digit }.
-- digit    = "0" | "1" | ... | "8" | "9".
-- expr     = term      { addop term    }.
-- term     = factor    { mulop factor  }.
-- factor   = "(" expr ")" | number.
-- addop    = "+" | "-"
-- mulop    = "*"


-- chainl              ::  Parser a -> Parser (a -> a -> a) -> a -> Parser a
-- chainl pa pop a     =   (pa `chainl1` pop) <|> return a

