{-# OPTIONS_GHC -w #-}
module Parser (parseProp) where

import Proposition
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4

action_0 (5) = happyShift action_4
action_0 (6) = happyShift action_5
action_0 (7) = happyShift action_6
action_0 (12) = happyShift action_7
action_0 (14) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (14) = happyShift action_2
action_1 _ = happyFail

action_2 (5) = happyShift action_4
action_2 (6) = happyShift action_5
action_2 (7) = happyShift action_6
action_2 (12) = happyShift action_7
action_2 (14) = happyShift action_2
action_2 (4) = happyGoto action_14
action_2 _ = happyFail

action_3 (8) = happyShift action_9
action_3 (9) = happyShift action_10
action_3 (10) = happyShift action_11
action_3 (11) = happyShift action_12
action_3 (13) = happyShift action_13
action_3 (16) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_8

action_5 _ = happyReduce_9

action_6 _ = happyReduce_10

action_7 (5) = happyShift action_4
action_7 (6) = happyShift action_5
action_7 (7) = happyShift action_6
action_7 (12) = happyShift action_7
action_7 (14) = happyShift action_2
action_7 (4) = happyGoto action_8
action_7 _ = happyFail

action_8 _ = happyReduce_2

action_9 (5) = happyShift action_4
action_9 (6) = happyShift action_5
action_9 (7) = happyShift action_6
action_9 (12) = happyShift action_7
action_9 (14) = happyShift action_2
action_9 (4) = happyGoto action_20
action_9 _ = happyFail

action_10 (5) = happyShift action_4
action_10 (6) = happyShift action_5
action_10 (7) = happyShift action_6
action_10 (12) = happyShift action_7
action_10 (14) = happyShift action_2
action_10 (4) = happyGoto action_19
action_10 _ = happyFail

action_11 (5) = happyShift action_4
action_11 (6) = happyShift action_5
action_11 (7) = happyShift action_6
action_11 (12) = happyShift action_7
action_11 (14) = happyShift action_2
action_11 (4) = happyGoto action_18
action_11 _ = happyFail

action_12 (5) = happyShift action_4
action_12 (6) = happyShift action_5
action_12 (7) = happyShift action_6
action_12 (12) = happyShift action_7
action_12 (14) = happyShift action_2
action_12 (4) = happyGoto action_17
action_12 _ = happyFail

action_13 (5) = happyShift action_4
action_13 (6) = happyShift action_5
action_13 (7) = happyShift action_6
action_13 (12) = happyShift action_7
action_13 (14) = happyShift action_2
action_13 (4) = happyGoto action_16
action_13 _ = happyFail

action_14 (8) = happyShift action_9
action_14 (9) = happyShift action_10
action_14 (10) = happyShift action_11
action_14 (11) = happyShift action_12
action_14 (13) = happyShift action_13
action_14 (15) = happyShift action_15
action_14 _ = happyFail

action_15 _ = happyReduce_1

action_16 (8) = happyShift action_9
action_16 (9) = happyShift action_10
action_16 (10) = happyShift action_11
action_16 (11) = happyShift action_12
action_16 (13) = happyShift action_13
action_16 _ = happyReduce_7

action_17 (8) = happyShift action_9
action_17 (9) = happyShift action_10
action_17 (10) = happyShift action_11
action_17 (11) = happyShift action_12
action_17 _ = happyReduce_6

action_18 (8) = happyShift action_9
action_18 (9) = happyShift action_10
action_18 (10) = happyShift action_11
action_18 (11) = happyShift action_12
action_18 _ = happyReduce_5

action_19 (8) = happyShift action_9
action_19 (9) = happyShift action_10
action_19 _ = happyReduce_4

action_20 (8) = happyShift action_9
action_20 _ = happyReduce_3

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (neg happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1 /\ happy_var_3)
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1 \/ happy_var_3)
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1 ==> happy_var_3)
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1 <== happy_var_3)
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1 <=> happy_var_3)
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  4 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn4
		 (T
	)

happyReduce_9 = happySpecReduce_1  4 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn4
		 (F
	)

happyReduce_10 = happySpecReduce_1  4 happyReduction_10
happyReduction_10 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Atom happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 16 16 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenT -> cont 5;
	TokenF -> cont 6;
	TokenVar happy_dollar_dollar -> cont 7;
	TokenAnd -> cont 8;
	TokenOr -> cont 9;
	TokenImp -> cont 10;
	TokenBImp -> cont 11;
	TokenNot -> cont 12;
	TokenEq -> cont 13;
	TokenOB -> cont 14;
	TokenCB -> cont 15;
	_ -> happyError' (tk:tks)
	}

happyError_ 16 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => E a -> (a -> E b) -> E b
happyThen = (thenE)
happyReturn :: () => a -> E a
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> E a
happyReturn1 = \a tks -> (returnE) a
happyError' :: () => [(Token)] -> E a
happyError' = parseError

parse tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- Used for error handling
data E a = Ok a | Failed String


thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
   case m of
     Ok a -> k a
     Failed e -> Failed e


returnE :: a -> E a
returnE a = Ok a


failE :: String -> E a
failE err = Failed err


catchE :: E a -> (String -> E a) -> E a
catchE m k =
   case m of
      Ok a -> Ok a
      Failed e -> k e


parseError _ = failE "Parse error"


data Token =
  TokenT | TokenF | TokenAnd | TokenOr |
  TokenImp | TokenBImp | TokenNot | TokenEq |
  TokenOB | TokenCB | TokenVar String
  deriving Show

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('/':'\\':cs) = TokenAnd : tokenize cs
tokenize ('&':cs) = TokenAnd : tokenize cs
tokenize ('\\':'/':cs) = TokenOr : tokenize cs
tokenize ('|':cs) = TokenOr : tokenize cs
tokenize ('<':'-':'>':cs) = TokenEq : tokenize cs
tokenize ('<':'=':'>':cs) = TokenEq : tokenize cs
tokenize ('-':'>':cs) = TokenImp : tokenize cs
tokenize ('=':'>':cs) = TokenImp : tokenize cs
tokenize ('<':'-':cs) = TokenBImp : tokenize cs
tokenize ('<':'=':cs) = TokenBImp : tokenize cs
tokenize ('-':cs) = TokenNot : tokenize cs
tokenize ('~':cs) = TokenNot : tokenize cs
tokenize ('(':cs) = TokenOB : tokenize cs
tokenize (')':cs) = TokenCB : tokenize cs
tokenize ('T':cs) = TokenT : tokenize cs
tokenize ('F':cs) = TokenF : tokenize cs
tokenize input@(c:cs)
  | isVarChar c = TokenVar var : tokenize rest
  where
    isVarChar = (`elem` "abcdefghijklmnopqrstuvwxyz")
    (var, rest) = span isVarChar input
-- just skip unrecognizable characters
tokenize (_:cs) = tokenize cs


parseProp str = case parse $ tokenize str of
  Ok p -> Right p
  Failed s -> Left s
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc-8.0.2/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc3379_0/ghc_2.h" #-}


























































































































































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

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

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
