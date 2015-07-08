module Expresion where

import Vallue
import Lambda
import Names
import Info
import Associativity

type Expresion = LamTerm Loc Name

getpres :: LamTerm i n -> (Precedence, Associativity)
getpres (Val _ BuildIn {fixity = InFix p a}) = (p, a)
getpres _ = (11, AssoLeft)

parensIf :: Bool -> String -> String
parensIf True string = parens string
parensIf False string = string

parens :: String -> String
parens string = "(" ++ string ++ ")"

pShow :: LamTerm i Name -> String
pShow = go (0 :: Int) where
  go _ (Var _ (Name n)) = n
  go _ (Val _ v) = pShowVal v
  go b (Lambda _ (Name "#") t) = go b t
  go b (Lambda _ (Name n) t) = parensIf (b > 0) $ "\\" ++ n ++ "." ++ go b t
  go b (Appl _ t1 t2 ) = parensIf (b > 1) $ case t1 of
        (Val _ v@(BuildIn {fixity = InFix p a})) -> case t2 of
            (Var _ (Name "#")) -> pShowVal v
            _ -> go 1 t2 ++ " " ++ pShowVal v
        _ -> go 1 t1 ++ " " ++ case t2 of
            (Val _ v@(BuildIn {fixity = InFix p a})) -> parens (pShowVal v)
            _ -> go 2 t2
