module Data.Json 
    (
    JValue(..)
    , getString
    , getInt
    , getDouble
    , getObject
    , getArray
    , isNull
    ) where

import Data.List (intercalate)
import Parser.Core

data JValue     = JString String
                | JNumber Double
                | JBool Bool
                | JNull
                | JObject [(String, JValue)] 
                | JArray [JValue] 
                    deriving (Eq, Ord)

instance Show JValue where
    show (JString s)    = show s
    show (JNumber n)    = show n
    show (JBool True)   = "true"
    show (JBool False)  = "false"
    show JNull          = "null"
    show (JObject o)    = "{" ++ pairs o ++ "}" 
        where   pairs []    = ""
                pairs ps    = intercalate ", " (map renderPair ps)
                renderPair (k,v) = show k ++  ": " ++ show v


instance Read JValue where
    readsPrec _ str     = run str

jstring ""      = []
jstring (c:cs)  = undefined

json = undefined 
run = runParser json 






-- pp 147
-- pp 148 
 


getString :: JValue -> Maybe String
getString (JString s)   = Just s
getString _             = Nothing

getInt (JNumber n)      = Just (truncate n)
getInt _                = Nothing
getDouble (JNumber n)   = Just n
getDouble _             = Nothing
getBool  (JBool b)      = Just b
getBool  _              = Nothing
getObject (JObject o)   = Just o
getObject _             = Nothing
getArray (JArray a)     = Just a
getArray _              = Nothing
isNull  v               = v == JNull





