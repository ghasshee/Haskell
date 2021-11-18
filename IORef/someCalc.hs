import Data.IORef 
import Control.Monad

someCalc :: IO Int 
someCalc = do 
    sum <- newIORef (0::Int)
    forM_ [0 .. 100] $ \i -> do 
        modifyIORef' sum (\sum -> sum + i)
    readIORef sum

