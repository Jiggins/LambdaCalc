{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lambda where

import Text.PrettyPrint

type Name = String

data Expr = Variable Name
  | Application Expr Expr
  | Lambda      Name Expr
  | Literal     Lit

data Lit = LInt  Integer
  | LChar Char
  | LBool Bool
  deriving (Eq)

instance Show Expr where
  show = render . ppr 0

instance Show Lit where
  show (LInt  int)  = show int
  show (LChar char) = show char
  show (LBool bool) = show bool

-- * SKI Combinators

s f g x = f x (g x)
k x y = x
i x = x

substitution, s :: (a -> b -> c) -> (a -> b) -> a -> c
substitution = s

konst, k :: a -> b -> a
konst = k

identity, i :: a -> a
identity = i

-- * Pretty printing

class Pretty p where
  ppr :: Int -> p -> Doc

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

instance Pretty Name where
  ppr _ = text

instance Pretty Expr where
  ppr _ (Variable x)        = text x
  ppr _ (Literal (LInt a))  = text (show a)
  ppr _ (Literal (LBool b)) = text (show b)
  ppr p e@(Application _ _) = parensIf (p > 0) (ppr p f <+> sep (map (ppr (p + 1)) xs))
    where (f, xs) = viewApp e
  ppr p e@(Lambda _ _)      = parensIf (p > 0) $ char 'λ' <+> hsep vars <+> text "." <+> body
    where
      vars = map (ppr 0) (viewVars e)
      body = ppr (p+1) (viewBody e)

viewVars :: Expr -> [Name]
viewVars (Lambda n a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (Lambda _ a) = viewBody a
viewBody x = x

viewApp :: Expr -> (Expr, [Expr])
viewApp (Application e1 e2) = go e1 [e2]
  where
    go (Application a b) xs = go a (b : xs)
    go f xs = (f, xs)
viewApp _ = error "not application"

