import Data.Number.Dif
import Data.List
import System.IO
import System.Cmd

tangent f a = 
    \x -> (deriv f a) * (x - a) * (unDif f a)

main = 
    let 
        xmin = -2*pi
        xmax = 2*pi
        step = 0.1
        f(x) = sin x 
        funs = [f, deriv f, tangent f 1.0, tangent f 2.0]
    in 
    let 
        range   = [xmin, xmin+step .. xmax]
        graphs  = [map f range | f <- funs]
        datas   = transpose (range : graphs)
    in
    do 
        (datafile, hd) <- openTempFile "/tmp" "diff-data"
        mapM (hPutStrLn hd) $
            [concat . intersperse "\t" . map show $ d | d<- datas]
        hFlush hd

        (cmdfile, hc) <- openTempFile "/tmp" "diff-command"
        hPutStrLn hc"set xzeroaxis"
        hPutStrLn hc"set yzeroaxis"
        hPutStrLn hc $ "plot " ++ 
            ( concat . intersperse "," 
            . map (\n-> show datafile ++ " using 1:" ++ show n ++ " w l")) [2 .. length graphs+1]
        hPutStrLn hc"pause -1"
        hFlush hc

        system $ "gnuplot " ++ cmdfile
