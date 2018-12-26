{-# OPTIONS_HADDOCK prune, ignore-exports #-}
-- | Exporter for proofs
module G4ipProver.LaTeXExporter (proofToString, exportContexts)
where


import Data.List (nub, elemIndex)

import G4ipProver.Prover
import G4ipProver.Proposition


-- | Data type representing proof structure (without rules).
data BussTree =
  Axiom String |
  Unary String BussTree (Maybe String) |
  Binary String BussTree BussTree (Maybe String)


-- | Export BussTree to LaTeX
treeToLatex :: BussTree -> String
treeToLatex (Axiom s) = "\\AxiomC{ " ++ s ++ " }\n"
treeToLatex (Unary s t l) =
  treeToLatex t ++ addLabel l ++ "\\UnaryInfC{ " ++ s ++ "}\n"
treeToLatex (Binary s t1 t2 l) =
  treeToLatex t1 ++ treeToLatex t2 ++ addLabel l ++ "\\BinaryInfC{ " ++ s ++ "}\n"


-- | Add @\\RightLabel{ ... }@ to tree node.
addLabel :: Maybe String -> String
addLabel = maybe "" (\s -> "\\RightLabel{" ++ s ++ "}")


downarrow :: BussTree
downarrow = Axiom " $\\downarrow$ "


turnstile :: String
turnstile = " $\\vdash$ "


-- | Export context to LaTeX
showContext :: Context -> String
showContext p =
  let lst = map printProp (uncurry (++) p) in
    if null lst then
      " $\\emptyset$ "
    else
      foldl1 (\x y -> x ++ ", " ++ y) lst


-- | Convert proposition to string.
printProp :: Prop -> String
printProp (Atom x) = x
printProp T = " $ \\top $ "
printProp F = " $ \\bot $ "
printProp (And a b) =
  "(" ++ printProp a ++ " $\\wedge$ " ++ printProp b ++ ")"
printProp (Or a b) =
  "(" ++ printProp a ++ " $\\vee$ " ++ printProp b ++ ")"
printProp (Imp a b) = "(" ++ printProp a ++ " $\\rightarrow$ " ++ printProp b ++ ")"


-- | Convert proof tree to structural tree
printProof :: ProofTree String -> BussTree
printProof (TopR _) =
  (Unary
    (" $\\emptyset$ " ++ turnstile ++ " $\\top$ ")
    downarrow
    (Just "($\\top R$)"))
printProof (BottomL ctx c) =
  (Unary
    (ctx ++ ", $\\bot$ " ++ turnstile ++ printProp c)
    downarrow
    (Just "($\\bot L$)"))
printProof (SplitAnd ctx a b ap bp) =
  (Binary
    (ctx ++ turnstile ++ printProp (And a b))
    (printProof ap)
    (printProof bp)
    (Just "($\\wedge R$)"))
printProof (ImpRight ctx a b pa) =
  (Unary
    (ctx ++ turnstile ++ printProp a ++ " $\\rightarrow$ " ++ printProp b)
    (printProof pa)
    (Just "($\\rightarrow R$)"))
printProof (AndLeft ctx a b c p) =
  (Unary
   (ctx ++ ", " ++ printProp (And a b) ++ turnstile ++ printProp c)
   (printProof p)
   (Just "($\\wedge L$)"))
printProof (ElimOr ctx a b c pa pb) =
  (Binary
    (ctx ++ ", " ++ printProp (Or a b) ++ turnstile ++ printProp c)
    (printProof pa)
    (printProof pb)
    (Just "($\\vee L$)"))
printProof (TImpLeft ctx b c p) =
  (Unary
   (ctx ++ ", " ++ printProp (Imp T b) ++ turnstile ++ printProp c)
   (printProof p)
   (Just "($\\top \\rightarrow L$)"))
printProof (FImpLeft ctx a c p) =
  (Unary
    (ctx ++ ", " ++ printProp a ++ turnstile ++ printProp c)
    (printProof p)
    (Just "($\\bot \\rightarrow L$)"))
printProof (AndImpLeft ctx d e b c p) =
  (Unary
    (ctx ++ ", " ++ printProp (Imp (And d e) b) ++ turnstile ++ printProp c)
    (printProof p)
    (Just "($\\wedge \\rightarrow L$)"))
printProof (OrImpLeft ctx d e b c p) =
  (Unary
    (ctx ++ ", " ++ printProp (Imp (Or d e) b) ++ turnstile ++ printProp c)
    (printProof p)
    (Just "($\\vee \\rightarrow L$)"))
printProof (OrRight1 ctx a b p) =
  (Unary
    (ctx ++ ", " ++ turnstile ++ printProp (Or a b))
    (printProof p)
    (Just "($\\vee R_1$)"))
printProof (OrRight2 ctx a b p) =
  (Unary
    (ctx ++ ", " ++ turnstile ++ printProp (Or a b))
    (printProof p)
    (Just "($\\vee R_2$)"))
printProof (LeftBoth ctx a p) = printProof p
printProof (InitRule ctx a) =
  (Unary
    (ctx ++ ", " ++ printProp a ++ turnstile ++ printProp a)
    downarrow
    (Just "(init rule)"))
printProof (PImpLeft ctx s b c p1 p2) =
  (Binary
    (ctx ++ ", " ++ printProp (Imp s b) ++ turnstile ++ printProp c)
    (printProof p1)
    (printProof p2)
    (Just "($P \\rightarrow L$)"))
printProof (ImpImpLeft ctx d e b c p1 p2) =
  (Binary
    (ctx ++ ", " ++ printProp (Imp (Imp d e) b) ++ turnstile ++ printProp c)
    (printProof p1)
    (printProof p2)
    (Just "($\\rightarrow \\rightarrow L$)"))


-- | Extract unique contexts
getContexts :: (ProofTree Context) -> [Context]
getContexts = nub . filter (/=([], [])) . extractContexts
  where
    extractContexts :: (ProofTree Context) -> [Context]
    extractContexts (TopR ctx) = [ctx]
    extractContexts (BottomL ctx _) = [ctx]
    extractContexts (SplitAnd ctx _ _ a b) = ctx : extractContexts a ++ extractContexts b
    extractContexts (ImpRight ctx _ _ a) = ctx : extractContexts a
    extractContexts (AndLeft ctx _ _ _ a) = ctx : extractContexts a
    extractContexts (ElimOr ctx _ _ _ a b) = ctx : extractContexts a ++ extractContexts b
    extractContexts (TImpLeft ctx _ _ a) = ctx : extractContexts a
    extractContexts (FImpLeft ctx _ _ a) = ctx : extractContexts a
    extractContexts (AndImpLeft ctx _ _ _ _ a) = ctx : extractContexts a
    extractContexts (OrImpLeft ctx _ _ _ _ a) = ctx : extractContexts a
    extractContexts (OrRight1 ctx _ _ a) = ctx : extractContexts a
    extractContexts (OrRight2 ctx _ _ a) = ctx : extractContexts a
    extractContexts (LeftBoth ctx _ a) = ctx : extractContexts a
    extractContexts (InitRule ctx _) = [ctx]
    extractContexts (PImpLeft ctx _ _ _ a b) = ctx : extractContexts a ++ extractContexts b
    extractContexts (ImpImpLeft ctx _ _ _ _ a b) = ctx : extractContexts a ++ extractContexts b


-- | Convert proof tree to string
proofToString :: ProofTree Context -> String
proofToString proofTree =
  treeToLatex (printProof . nameContexts proofTree $ getContexts proofTree)


-- | Export contexts from proof tree to string
exportContexts :: ProofTree Context -> String
exportContexts proofTree =
  foldl (\x y -> x ++ "\n\n" ++ y) "" .
  map (\(s, n) -> "$\\Gamma_{" ++ show (n :: Integer) ++ "} = $" ++ s) $
  zip (map showContext $ getContexts proofTree) [1..]


-- | Replace all contexts in proof tree with their names
nameContexts :: ProofTree Context -> [Context] -> ProofTree String
nameContexts (TopR ctx) ctxs =
  TopR (getName ctxs ctx)
nameContexts (BottomL ctx a) ctxs =
  BottomL (getName ctxs ctx) a
nameContexts (SplitAnd ctx a b c d) ctxs =
  SplitAnd (getName ctxs ctx) a b (nameContexts c ctxs) (nameContexts d ctxs)
nameContexts (ImpRight ctx a b c) ctxs =
  ImpRight (getName ctxs ctx) a b (nameContexts c ctxs)
nameContexts (AndLeft ctx a b c d) ctxs =
  AndLeft (getName ctxs ctx) a b c (nameContexts d ctxs)
nameContexts (ElimOr ctx a b c d e) ctxs =
  ElimOr (getName ctxs ctx) a b c (nameContexts d ctxs) (nameContexts e ctxs)
nameContexts (TImpLeft ctx a b c) ctxs =
  TImpLeft (getName ctxs ctx) a b (nameContexts c ctxs)
nameContexts (FImpLeft ctx a b c) ctxs =
  FImpLeft (getName ctxs ctx) a b (nameContexts c ctxs)
nameContexts (AndImpLeft ctx a b c d e) ctxs =
  AndImpLeft (getName ctxs ctx) a b c d (nameContexts e ctxs)
nameContexts (OrImpLeft ctx a b c d e) ctxs =
  OrImpLeft (getName ctxs ctx) a b c d (nameContexts e ctxs)
nameContexts (OrRight1 ctx a b c) ctxs =
  OrRight1 (getName ctxs ctx) a b (nameContexts c ctxs)
nameContexts (OrRight2 ctx a b c) ctxs =
  OrRight2 (getName ctxs ctx) a b (nameContexts c ctxs)
nameContexts (LeftBoth ctx a b) ctxs =
  LeftBoth (getName ctxs ctx) a (nameContexts b ctxs)
nameContexts (InitRule ctx a) ctxs =
  InitRule (getName ctxs ctx) a
nameContexts (PImpLeft ctx a b c d e) ctxs =
  PImpLeft (getName ctxs ctx) a b c (nameContexts d ctxs) (nameContexts e ctxs)
nameContexts (ImpImpLeft ctx a b c d e f) ctxs =
  ImpImpLeft (getName ctxs ctx) a b c d (nameContexts e ctxs) (nameContexts f ctxs)


-- | Get context name from list of contexts
getName :: [Context] -> Context -> String
getName xs ctx =
  if ctx == ([], []) then
    " $\\emptyset$ "
  else
    case elemIndex ctx xs of
      Just ind -> " $\\Gamma_{" ++ show (ind + 1) ++ "}$"
      Nothing -> show ctx
