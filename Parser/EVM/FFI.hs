module FFI where 

import Foreign
import Foreign.C.Types

foreign import ccall "math.h sin" 
    c_sin :: CDouble -> CDouble


foreign import ccall "math.h cos" 
    c_cos :: CDouble -> CDouble 


foreign import ccall "stdio.h printf"
    c_printf :: CInt -> CInt 

