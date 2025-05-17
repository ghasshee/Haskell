module Aop.Stream where

{-# LANGUAGE ScopedTypeVariables #-} 

import Data.Char

data List a = 
      Cons (a, List a) 
    | Nil 

{--
data Stream b = 
      SCons (b, Stream b)
--}
{--
data Stream b = 
      SCons { runStream :: ( b, Stream b) } 
--}


-- with newtype, the Compiler can more easily optimize 
newtype Stream b = 
      SCons { runStream :: (b, Stream b) }  deriving Show 


myList :: List Int 
myList = Cons (1,Cons (2,Cons (3,Nil)))

myList' :: [Int]
myList' = 1:2:3:[] 


myStream :: Stream Int 
myStream = streamFrom 1  
    where 
        streamFrom :: Int -> Stream Int 
        streamFrom n = SCons (n, streamFrom (n+1))     

{-- The above definition is meaningful because Haskell is lazy 
 - in ML, we can use () unit type to do the same thing. --} 


stream2list :: Stream b -> [b] 
stream2list (SCons(x,xs)) = x: stream2list xs 



testStream :: Stream b -> Int -> ([b], Stream b) 
testStream strm 0 = ([],strm) 
testStream strm n = (y:ys, final) 
    where 
        (y, next)   = runStream strm 
        (ys, final) = testStream next (n-1) 

testStream_ = (.)(.)(.) fst testStream 

test1 = take 10 $ stream2list myStream 
test2 = testStream_ myStream 10 



streamFrom :: Int -> Stream Int
streamFrom n = SCons (n , streamFrom (n+1) ) 

charStream :: Stream Char 
charStream = charStreamFrom 65
    where 
        charStreamFrom :: Int -> Stream Char
        charStreamFrom n = SCons (chr n, charStreamFrom (n+1))


a = take 100 $ stream2list charStream 




newtype Auto a b = ACons { runAuto :: a -> (b,Auto a b) } 

myStreamAuto :: Auto a Int
myStreamAuto = streamAutoFrom 1 
    where 
        streamAutoFrom :: Int -> Auto a Int 
        streamAutoFrom n = ACons $ \_ -> (n,streamAutoFrom (n+1) )


settableAuto :: Auto (Maybe Int) Int
settableAuto = counterFrom 1 
    where 
        counterFrom :: Int -> Auto ( Maybe Int ) Int 
        counterFrom n = ACons $ \reset -> 
            let c = fromMaybe n reset in 
            (c, counterFrom (c+1) ) 


fromMaybe d (Just a) = a 
fromMaybe d Nothing  = d 



testAuto :: Auto a b -> [a] -> ([b], Auto a b) 
testAuto auto []        = ([],auto) 
testAuto auto (x:xs)    = (y:ys, final) 
    where 
        (y,next)    = runAuto auto x
        (ys,final)  = testAuto next xs 

testAuto_ = (.)(.)(.) fst testAuto 


test3 = testAuto_ settableAuto [Nothing, Nothing , Nothing , Just 100 , Nothing, Nothing, Just (-1), Nothing , Nothing ]  


interactAuto :: (Read a, Show b) => Auto a b -> IO () 
interactAuto a0 = do 
    inp <- getLine
    let (x,a1) = runAuto a0 (read inp) 
    print x 
    interactAuto a1 


isEvenAuto :: Auto (Maybe Int) Bool
isEvenAuto = isEvenAutoFrom 1 
    where 
        isEvenAutoFrom :: Int -> Auto (Maybe Int) Bool
        isEvenAutoFrom n = ACons $ \reset -> 
            let c = fromMaybe n reset in 
            ( even c, isEvenAutoFrom (c + 1) ) 

test4  = testAuto_ isEvenAuto [ Nothing , Nothing , Just 10 , Nothing , Nothing , Just(-1), Nothing ]


summer :: Num a => Auto a a 
summer = sumFrom 0 
    where 
        sumFrom :: Num a => a -> Auto a a 
        sumFrom n = ACons $ \input -> 
            let s = n + input in 
            ( s , sumFrom s ) 



test5 = 
    let (x, au)  = runAuto summer 10 in 
    let (y, au') = runAuto au 3 in 
    testAuto_ au' [30,20,-10,5,7,-9,100,24]


foldAuto :: forall a b . (b -> a -> b) -> b -> Auto a b 
foldAuto op init = foldFrom init 
    where 
        foldFrom x = ACons $ \input -> 
            let y = x `op` input in 
            (y, foldFrom y) 

accumulateInt2List :: Auto a [a] 
accumulateInt2List = foldAuto (flip (:)) []

productor :: Num a => Auto a a 
productor = foldAuto (*) 1 

accumulateStrings :: Auto String String 
accumulateStrings = foldAuto (++) "" 

accumulateStringsWithSpace :: Auto String String 
accumulateStringsWithSpace = foldAuto (\s t -> s ++ " " ++ t ) "" 

monoidAccum :: Monoid a => Auto a a 
monoidAccum = foldAuto mappend mempty 


test6 = testAuto_ productor [1..10] 
test7 = testAuto_ accumulateStringsWithSpace ["Hello", "John", "Steeven", "How are you?"] 
test8 = testAuto_ monoidAccum ["car", "d", "inal", "ity" ] 


-- exercise : Intro to Machines & Arrows (Part 1 : Stream and Auto ) 
--
--
