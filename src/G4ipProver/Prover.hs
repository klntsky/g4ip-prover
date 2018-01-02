-- | The actual theorem prover
module G4ipProver.Prover
  (
    prove,
    decide,
    ProofTree(..),
    Context
  )
where


import Control.Arrow (second)
import Data.List (inits, tails)
import Data.Tuple (uncurry)
import Control.Monad (liftM2)
import Data.Maybe (catMaybes, isJust, listToMaybe)


import G4ipProver.Proposition (Prop (..))


-- | Construct a proof if it exists for the given proposition.
prove :: Prop -> Maybe (ProofTree Context)
prove = right ([], [])


-- | Decide if the given proposition has a proof
decide :: Prop -> Bool
decide = isJust . prove


-- | Type parameter represents context type.
data ProofTree a =
  -- | @init@ rule: @Γ, P ⊢ P@
  InitRule a Prop |
  -- | @⊤R@ rule: @∅ ⊢ T@
  TopR a |
  -- | @⊥L@ rule: @Γ, ⊥ ⊢ A@
  BottomL a Prop |
  -- | @∧R@ rule
  SplitAnd a Prop Prop (ProofTree a) (ProofTree a) |
  -- | @→R@ rule
  ImpRight a Prop Prop (ProofTree a) |
  -- | @∧L@ rule
  AndLeft a Prop Prop Prop (ProofTree a) |
  -- |
  ElimOr a Prop Prop Prop (ProofTree a) (ProofTree a) |
  -- | @⊤L@ rule
  TImpLeft a Prop Prop (ProofTree a) |
  -- | @⊥→L@ rule
  FImpLeft a Prop Prop (ProofTree a) |
  -- | @∧→L@ rule
  AndImpLeft a Prop Prop Prop Prop (ProofTree a) |
  -- | @∨→L@ rule
  OrImpLeft a Prop Prop Prop Prop (ProofTree a) |
  -- | @∨R1@ rule
  OrRight1 a Prop Prop (ProofTree a) |
  -- | @∨R2@ rule
  OrRight2 a Prop Prop (ProofTree a) |
  LeftBoth a Prop (ProofTree a) |
  -- | @P→L@ rule
  PImpLeft a Prop Prop Prop (ProofTree a) (ProofTree a) |
  -- | @→→L@ rule
  ImpImpLeft a Prop Prop Prop Prop (ProofTree a) (ProofTree a)
  deriving (Show)


-- | Context is a tuple of (invertible propositions, non-invertible propositions).
type Context = ([Prop], [Prop])


-- | Add a proposition to the context.
add :: Prop -> Context -> Context
add p ctx@(inv, other) = case p of
  Atom _ -> (inv, p : other)   -- Assume not-invertible
  T -> ctx                     -- Leave out since useless
  F -> ([F], [])               -- Do not need anything else
  And _ _ -> (p : inv, other)
  Or _ _ -> (p : inv, other)
  Imp (Atom _) _ -> (inv, p : other)
  Imp (Imp _ _) _ -> (inv, p : other)
  Imp _ _ -> (p : inv, other)


-- | Invertible decisions.
right :: Context -> Prop -> Maybe (ProofTree Context)
right ctx T = Just (TopR ctx)
right ctx (And a b) =
  liftM2 (SplitAnd ctx a b) (right ctx a) (right ctx b)
right ctx (Imp a b) =
  right (add a ctx) b >>=
  Just . ImpRight ctx a b
right ctx@(And a b : inv, other) c =
  right (add a $ add b (inv, other)) c >>=
  Just . AndLeft ctx a b c
right ctx@(F : _, a) c = Just $ BottomL ctx c
right ctx@(Or a b : inv, other) c =
  right (add a (inv, other)) c >>=
  (\p1 -> right (add b (inv, other)) c >>=
    Just . ElimOr ctx a b c p1)
right ctx@(Imp T b : inv, other) c =
  right (add b (inv, other)) c >>= Just . TImpLeft ctx b c
right ctx@(Imp F a : inv, other) c =
  right (inv, other) c >>= Just . FImpLeft ctx a c
right ctx@(Imp (And d e) b : inv, other) c =
  right (add (Imp d $ Imp e b) (inv, other)) c >>=
  Just . AndImpLeft ctx d e b c
right ctx@(Imp (Or d e) b : inv, other) c =
  right (add (Imp e b) $ add (Imp d b) (inv, other)) c >>=
  Just . OrImpLeft ctx d e b c
right ctx@([], other) t@(Or a b) =
  case left other t of
    Just p -> Just (LeftBoth ctx t p)
    Nothing -> case right ([], other) a of
      Just p -> Just (OrRight1 ctx a b p)
      Nothing -> case right ([], other) b of
        Just p -> Just (OrRight2 ctx a b p)
        Nothing -> Nothing
right ([], other) c =
  left other c


-- | Non-invertible decisions
left :: [Prop] -> Prop -> Maybe (ProofTree Context)
left other c = listToMaybe . catMaybes . map (`elim` c) $ pulls other
  where
    -- | Pull one element out for all elements. For example,
    --
    -- > pulls "abc" == [('a',"bc"),('b',"ac"),('c',"ab")]
    pulls :: [a] -> [(a, [a])]
    pulls xs = take (length xs) $ zipWith (second . (++)) (inits xs) breakdown
      where pull (x : xs) = (x, xs)
            breakdown = map pull (tails xs)


elim :: (Prop, [Prop]) -> Prop -> Maybe (ProofTree Context)
elim (Atom s1, ctx) r@(Atom s2) =
  if s1 == s2 then
    Just (InitRule ([], ctx) r)
  else Nothing
elim (Atom s, _) _ = Nothing
elim (Imp sa@(Atom s) b, other) c =
  right ([], other) (Atom s) >>=
  \p1 -> right (add b ([], other)) c >>=
  Just . PImpLeft ([], other) sa b c p1
elim (Imp (Imp d e) b, other) c =
  right (add d $ add (Imp e b) ([], other)) e >>=
  \p1 -> right (add b ([], other)) c >>=
  Just . ImpImpLeft ([], other) d e b c p1
