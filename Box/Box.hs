module Box.Box where

data BoxChar a = Box [[a]] | NewLine | EOB
data BoxString a = BoxString [BoxChar a]

instance (Show a) => Show (BoxString a) where
    show (BoxString line) = showBoxString (BoxString (line ++ [EOB]))

showBoxString (BoxString line) = showTop line ++ showRest (rest line) ++ isNewLine line
showTop []                  = ""
showTop (EOB:xs)            = "\n"
showTop (NewLine:xs)        = "\n"
showTop ((Box[]):xs)        = showTop xs
showTop ((Box(t:r)):xs)     = showline t ++ showTop xs
showRest []                 = ""
showRest (EOB:xs)           = "\n"
showRest (NewLine:xs)       = "\n"
showRest x                  = showTop x ++ showRest (rest x)
isNewLine [EOB]             = "end of BoxString.\n"
isNewLine (NewLine:xs)      = "\n" ++ showBoxString (BoxString xs)
isNewLine (x:xs)            = "" ++ isNewLine xs 

rest ((Box(t:r)):xs)    = (Box r):(rest xs)
rest _                  = []
showline []     = ""
showline (x:xs) = show x ++ "\t" ++ showline xs


