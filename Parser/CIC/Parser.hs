{-# OPTIONS_GHC -w #-}
module Parser where 
 
import Syntax 
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,74) ([16256,68,128,0,0,4,0,0,7168,68,0,0,7168,68,7168,68,0,0,0,0,0,0,16256,68,0,0,16256,68,0,128,16384,0,0,0,0,0,0,0,0,32,0,32512,16256,68,0,0,32768,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16256,68,16256,68,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Tm","AppTm","ATm","Ty","lam","succ","pred","zero","true","false","if","then","else","all","exists","x","'.'","ty","':'","'('","')'","type","set","prop","truth","falsity","nat","bool","%eof"]
        bit_start = st * 32
        bit_end = (st + 1) * 32
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..31]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (8) = happyShift action_2
action_0 (9) = happyShift action_6
action_0 (10) = happyShift action_7
action_0 (11) = happyShift action_8
action_0 (12) = happyShift action_9
action_0 (13) = happyShift action_10
action_0 (14) = happyShift action_11
action_0 (19) = happyShift action_12
action_0 (23) = happyShift action_13
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (8) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (19) = happyShift action_19
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (32) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (11) = happyShift action_8
action_4 (12) = happyShift action_9
action_4 (13) = happyShift action_10
action_4 (19) = happyShift action_12
action_4 (23) = happyShift action_13
action_4 (6) = happyGoto action_18
action_4 _ = happyReduce_3

action_5 _ = happyReduce_4

action_6 (11) = happyShift action_8
action_6 (12) = happyShift action_9
action_6 (13) = happyShift action_10
action_6 (19) = happyShift action_12
action_6 (23) = happyShift action_13
action_6 (6) = happyGoto action_17
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (11) = happyShift action_8
action_7 (12) = happyShift action_9
action_7 (13) = happyShift action_10
action_7 (19) = happyShift action_12
action_7 (23) = happyShift action_13
action_7 (6) = happyGoto action_16
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_9

action_9 _ = happyReduce_10

action_10 _ = happyReduce_11

action_11 (8) = happyShift action_2
action_11 (9) = happyShift action_6
action_11 (10) = happyShift action_7
action_11 (11) = happyShift action_8
action_11 (12) = happyShift action_9
action_11 (13) = happyShift action_10
action_11 (14) = happyShift action_11
action_11 (19) = happyShift action_12
action_11 (23) = happyShift action_13
action_11 (4) = happyGoto action_15
action_11 (5) = happyGoto action_4
action_11 (6) = happyGoto action_5
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_12

action_13 (8) = happyShift action_2
action_13 (9) = happyShift action_6
action_13 (10) = happyShift action_7
action_13 (11) = happyShift action_8
action_13 (12) = happyShift action_9
action_13 (13) = happyShift action_10
action_13 (14) = happyShift action_11
action_13 (19) = happyShift action_12
action_13 (23) = happyShift action_13
action_13 (4) = happyGoto action_14
action_13 (5) = happyGoto action_4
action_13 (6) = happyGoto action_5
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (24) = happyShift action_22
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (15) = happyShift action_21
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_7

action_17 _ = happyReduce_6

action_18 _ = happyReduce_5

action_19 (22) = happyShift action_20
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (25) = happyShift action_25
action_20 (26) = happyShift action_26
action_20 (27) = happyShift action_27
action_20 (28) = happyShift action_28
action_20 (29) = happyShift action_29
action_20 (30) = happyShift action_30
action_20 (31) = happyShift action_31
action_20 (7) = happyGoto action_24
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (8) = happyShift action_2
action_21 (9) = happyShift action_6
action_21 (10) = happyShift action_7
action_21 (11) = happyShift action_8
action_21 (12) = happyShift action_9
action_21 (13) = happyShift action_10
action_21 (14) = happyShift action_11
action_21 (19) = happyShift action_12
action_21 (23) = happyShift action_13
action_21 (4) = happyGoto action_23
action_21 (5) = happyGoto action_4
action_21 (6) = happyGoto action_5
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_8

action_23 (16) = happyShift action_33
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (20) = happyShift action_32
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_13

action_26 _ = happyReduce_14

action_27 _ = happyReduce_15

action_28 _ = happyReduce_18

action_29 _ = happyReduce_19

action_30 _ = happyReduce_16

action_31 _ = happyReduce_17

action_32 (8) = happyShift action_2
action_32 (9) = happyShift action_6
action_32 (10) = happyShift action_7
action_32 (11) = happyShift action_8
action_32 (12) = happyShift action_9
action_32 (13) = happyShift action_10
action_32 (14) = happyShift action_11
action_32 (19) = happyShift action_12
action_32 (23) = happyShift action_13
action_32 (4) = happyGoto action_35
action_32 (5) = happyGoto action_4
action_32 (6) = happyGoto action_5
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (8) = happyShift action_2
action_33 (9) = happyShift action_6
action_33 (10) = happyShift action_7
action_33 (11) = happyShift action_8
action_33 (12) = happyShift action_9
action_33 (13) = happyShift action_10
action_33 (14) = happyShift action_11
action_33 (19) = happyShift action_12
action_33 (23) = happyShift action_13
action_33 (4) = happyGoto action_34
action_33 (5) = happyGoto action_4
action_33 (6) = happyGoto action_5
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_2

action_35 _ = happyReduce_1

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LCID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (\c -> TmAbs (happy_var_4 c)(happy_var_6(addVar c happy_var_2))
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (\c -> TmIf (happy_var_2 c)(happy_var_4 c)(happy_var_6 c)
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (\c -> TmApp (happy_var_1 c)(happy_var_2 c)
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  5 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (\c -> TmSucc (happy_var_2 c)
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (\c -> TmPred (happy_var_2 c)
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn6
		 (\c -> TmZero
	)

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn6
		 (\c -> TmTrue
	)

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn6
		 (\c -> TmFalse
	)

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyTerminal (LCID happy_var_1))
	 =  HappyAbsSyn6
		 (\c -> TmVar $ getVar c happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 (HappyTerminal (TYPE happy_var_1))
	 =  HappyAbsSyn7
		 (\c -> Type happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  7 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn7
		 (\c -> Set
	)

happyReduce_15 = happySpecReduce_1  7 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn7
		 (\c -> Prop
	)

happyReduce_16 = happySpecReduce_1  7 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn7
		 (\c -> Nat
	)

happyReduce_17 = happySpecReduce_1  7 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn7
		 (\c -> Bool
	)

happyReduce_18 = happySpecReduce_1  7 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn7
		 (\c -> PTrue
	)

happyReduce_19 = happySpecReduce_1  7 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn7
		 (\c -> PFalse
	)

happyNewToken action sts stk [] =
	action 32 32 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	LAMBDA -> cont 8;
	SUCC -> cont 9;
	PRED -> cont 10;
	ZERO -> cont 11;
	TRUE -> cont 12;
	FALSE -> cont 13;
	IF -> cont 14;
	THEN -> cont 15;
	ELSE -> cont 16;
	ALL -> cont 17;
	EXISTS -> cont 18;
	LCID happy_dollar_dollar -> cont 19;
	DOT -> cont 20;
	UCID happy_dollar_dollar -> cont 21;
	COLON -> cont 22;
	LPAREN -> cont 23;
	RPAREN -> cont 24;
	TYPE happy_dollar_dollar -> cont 25;
	SET -> cont 26;
	PROP -> cont 27;
	TRUTH -> cont 28;
	FALSITY -> cont 29;
	NAT -> cont 30;
	BOOL -> cont 31;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 32 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError _ = error "!Parse Error" 


type Var = String 

getVar              :: [(Var,Bind)] -> Var -> Int 
getVar [] str       = error $ "cannot find " ++ str ++ " in context"
getVar (x:xs) str   = if fst x == str  then 0 else 1 + (getVar xs str)

addVar ctx str      = (str,NameBind):ctx
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.