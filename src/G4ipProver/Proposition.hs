-- | Definition of propositions and some syntactic sugar
module G4ipProver.Proposition (
    Prop (..)
  , (/\)
  , (\/)
  , (==>)
  , (<==)
  , (<=>)
  , neg) where


data Prop = Atom String
          | T
          | F
          | And Prop Prop
          | Or Prop Prop
          | Imp Prop Prop
            deriving (Eq, Ord)


infixr 3 /\
infixr 2 \/
infixr 1 ==>
infixr 1 <==
infixr 0 <=>


(/\) :: Prop -> Prop -> Prop
(/\) = And


(\/) :: Prop -> Prop -> Prop
(\/) = Or


(==>) :: Prop -> Prop -> Prop
(==>) = Imp


(<==) :: Prop -> Prop -> Prop
(<==) = flip Imp


(<=>) :: Prop -> Prop -> Prop
a <=> b = And (Imp a b) (Imp b a)


neg :: Prop -> Prop
neg a = a ==> F


instance Show Prop where
  show = showImp
    where
      showImp (Imp a b) = showAndOr a ++ " -> " ++ showAndOr b
      showImp p = showAndOr p

      showAndOr (And a b) = showAtom a ++ " /\\ " ++ showAtom b
      showAndOr (Or a b) = showAtom a ++ " \\/ " ++ showAtom b
      showAndOr p = showAtom p

      showAtom (Atom s) = s
      showAtom T = "T"
      showAtom F = "F"
      showAtom p = "(" ++ showImp p ++ ")"
