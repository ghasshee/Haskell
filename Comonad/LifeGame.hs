
module Main where 


import Comonad 
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay) 
import Data.List (intercalate) 



countNeighbours :: Z2 Bool -> Int 
countNeighbours (Z2 (Z
    (Z (ul:_) uc (ur:_) : _ ) 
    (Z (cl:_) cc (cr:_))
    (Z (dl:_) dc (dr:_) : _ ) 
    ))          = length $ filter id [ul,uc,ur,cl,cr,dl,dc,dr] 


life :: Z2 Bool -> Bool 
life z = (a && (n==2||n==3)) || (not a && n==3) 
    where
        n = countNeighbours z 
        a = extract z 

showZ2 :: Int -> Int -> Z2 Char -> IO () 
showZ2 w h (Z2 (Z _ _ rows)) = do 
    flip mapM_ (take h rows) $ \(Z _ _ row) -> do 
        putStrLn . intercalate " " . map pure $ take w row 


main    :: IO () 
main    = do 
    let c2b c       = if c == ' ' then False else True 
        b2c b       = if b then '#' else ' ' 
        (w,h)       = (20,20) 
        field       = [ " # "
                      , "  #"
                      , "###"] 
        initState   = fmap c2b $ toZ2 ' ' field 
        loop state  = do 
            let state' = extend life state 
            replicateM_ h $ putStr "\ESC[A\ESC[2K" -- clear terminal 
            showZ2 w h (fmap b2c state) 
            threadDelay 300000
            loop state' 

    replicateM_ h $ putStrLn "" 
    loop initState 

