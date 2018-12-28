module Main where

import G4ipProver

import System.Exit
import Control.Monad
import Control.Monad
import Data.Tuple (uncurry)

-- Most of the tests are from cacay/G4ip repo:
-- https://github.com/cacay/G4ip/

-- | A test is a proposition paired with whether or not it is provable
type Test = (Prop, Bool)

-- Some shortcuts
a = Atom "A"
b = Atom "B"
c = Atom "C"
d = Atom "D"
e = Atom "E"
f = Atom "F"
p = Atom "P"
s = Atom "S"
k = Atom "K"
l = Atom "L"

tests = [
    {- I -} ( a ==> a , True ),
    {- K -} ( a ==> (b ==> a) , True ),
    {- S -} ( (a ==> b) ==> (a ==> (b ==> c)) ==> (a ==> c) , True ),

    {- Peirce's law -- not provable, generally -}
    ( ((a ==> b) ==> a) ==> a , False ),
    {- ... but by Glivenko's theorem, its double-negation should be! -}
    ( neg (neg (((a ==> b) ==> a) ==> a)) , True ),

    {- andComm: -}
    ( a /\ b ==> b /\ a , True ),
    {- hw01, orComm: -}
    ( a \/ b ==> b \/ a , True ),

    {- hw01, clue: -}
    ( (p ==> (c /\ k) \/ (d /\ l))
         ==> (neg k ==> s)
         ==> (d \/ l)
         ==> (p ==> neg s) /\ (s ==> neg p)
         ==> (c ==> neg d) /\ (d ==> neg c)
         ==> (k ==> neg l) /\ (l ==> neg k)
         ==> neg p , True ),

    {- hw02, implOr: -}
    ( (a \/ c) /\ (b ==> c) ==> (a ==> b) ==> c, True ),

    {- some basic propositional (non-)tautologies -}
    ( (a ==> b ==> c) <=> (a /\ b ==> c) , True ),
    ( (a ==> b /\ c) <=> (a ==> b) /\ (a ==> c) , True ),
    ( (a ==> b \/ c) ==> (a ==> b) \/ (a ==> c) , False ),
    ( (a ==> b \/ c) <== (a ==> b) \/ (a ==> c) , True ),
    ( ((a ==> b) ==> c) ==> ((a \/ b) /\ (b ==> c)) , False ),
    ( ((a ==> b) ==> c) <== ((a \/ b) /\ (b ==> c)) , True ),
    ( (a \/ b ==> c) <=> (a ==> c) /\ (b ==> c) , True ),
    ( (a /\ (b \/ c)) <=> (a /\ b) \/ (a /\ c) , True ),
    ( (a \/ (b /\ c)) <=> (a \/ b) /\ (a \/ c) , True ),

    {- some deMorgan-like dualities -}
    ( neg (a /\ b) ==> neg a \/ neg b , False ),
    ( neg (a /\ b) <== neg a \/ neg b , True ),
    ( neg (a \/ b) <=> neg a /\ neg b , True ),
    ( neg (a ==> b) ==> a /\ neg b , False ),
    ( neg (a ==> b) <== a /\ neg b , True ),
    ( neg (neg a) ==> a , False ),
    ( neg (neg a) <== a , True ),
    ( neg T <=> F , True ),
    ( neg F <=> T , True ),

    {- triple-negation elimination -}
    ( neg (neg (neg a)) <=> neg a , True ),
    {- three-way transitivity -}
    ( (a ==> b) ==> (b ==> c) ==> (c ==> d) ==> (a ==> d) , True ),

    {- some test cases for various common mistakes -}

    ( (a ==> b) ==> (a ==> c) ==> a ==> b , True ),
    ( (a ==> b) ==> (a ==> c) ==> a ==> c , True ),
    ( a ==> (a ==> b) ==> (a ==> c) ==> b , True ),
    ( a ==> (a ==> b) ==> (a ==> c) ==> c , True ),

    ( (a ==> b ==> c) ==> a ==> b ==> c , True ),
    ( (a ==> b ==> c) ==> b ==> a ==> c , True ),
    ( a ==> b ==> (a ==> b ==> c) ==> c , True ),
    ( b ==> a ==> (a ==> b ==> c) ==> c , True ),

    {- it turns out that heavily left-nested instances of the identity
       theorem make really great stress-tests for correctness! -}
    ( (a ==> b) ==> a ==> b , True ),
    ( ((a ==> b) ==> c) ==> ((a ==> b) ==> c) , True ),
    ( (((a ==> b) ==> c) ==> d) ==> (((a ==> b) ==> c) ==> d) , True ),
    ( ((((a ==> b) ==> c) ==> d) ==> e) ==> (((a ==> b) ==> c) ==> d) ==> e , True ),
    ( (((((a ==> b) ==> c) ==> d) ==> e) ==> f) ==> ((((a ==> b) ==> c) ==> d) ==> e) ==> f , True ),
    ( (((((a ==> b) ==> c) ==> d) ==> e) ==> f) ==> (((((a ==> b) ==> c) ==> d) ==> e) ==> f) \/ (((((a ==> b) ==> c) ==> d) ==> e) ==> f) , True ),

    ( ((a ==> b) ==> c) ==> d ==> d \/ d, True )
  ]


-- | Result is 'True' if the test passes 'False' otherwise
check :: Prop -> Bool -> IO Bool
check p expected = do
  putStr $ "Checking " ++ show p ++ " " ++ expectation ++ "..."
  putStrLn (if correct then ok else wrong)
  return correct
  where
    expectation = if expected then "provable" else "unprovable"
    correct = decide p == expected

    ok :: String
    ok = "\027[1;30m[\027[32m OK! \027[30m]\027[m"

    wrong :: String
    wrong = "\027[1;30m[\027[31m WRONG \027[30m]\027[m"


-- | Run the given tests and report the number of errors
runTests :: [Test] -> IO Int
runTests tests = do
  results <- mapM (uncurry check) tests
  let errors = sum $ map (fromEnum . not) results
  putStrLn $ if errors == 0
    then "*** All tests passed!"
    else "*** Failed " ++ show errors ++ " out of " ++
      show (length tests) ++ " tests."
  return errors


main = do
  runTests tests
