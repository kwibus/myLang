module Expresion where

import Vallue
import Lambda
import Names
import Info
import Associativity

type Expresion = LamTerm Loc Name

getPres :: LamTerm i n -> (Precedence, Associativity)
getPres (Val _ BuildIn {fixity = InFix p a}) = (p, a)
getPres _ = highPres

getPresFuntion :: LamTerm i n -> (Precedence, Associativity)
getPresFuntion (Appl _ (Appl {}) _) = highPres
getPresFuntion (Appl _ term _ ) = getPres term
getPresFuntion term = getPres term

decrement :: (Precedence , Associativity) -> (Precedence , Associativity)
decrement (p, AssoLeft ) = (p, AssoRight)
decrement (p, AssoRight) = (p - 1, AssoLeft)

presAplicationfuction :: (Precedence, Associativity)
presAplicationfuction = (11, AssoLeft)

lowPres :: (Precedence, Associativity)
lowPres = (0, AssoLeft)

highPres :: (Precedence, Associativity)
highPres = (100, AssoLeft)

parensIf :: Bool -> String -> String
parensIf True string = parens string
parensIf False string = string

parens :: String -> String
parens string = "(" ++ string ++ ")"

pShow :: LamTerm i Name -> String
pShow = go False lowPres
 where
  go _ _ (Var _ (Name n)) = n
  go _ _ (Val _ v) = pShowVal v
  go b _ (Lambda _ (Name "#") t) = go b lowPres t
  go b p (Lambda _ (Name n) t) =
    let (vars, nextTerm) = acumulateVars t
    in parensIf b $ "\\" ++ unwords (filter (/= "#") (n : vars)) ++ "." ++ go b p nextTerm

  go b p (Appl _ t1 t2 )
    | isInfix t1 =
        let string2 = case t2 of
               (Var _ (Name "#")) -> ""
               _ -> mkString2 True (decrement (getPresFuntion t1))
        in myConcat string2 string1

    | otherwise = parensIf ( higerPres p (getPresFuntion t1)) $
        let string2 = mkString2 b (getPresFuntion t1)
        in myConcat string1 string2

    where string1 = go True presAplicationfuction t1
          mkString2 b p = parensIf (isNotFullAplliedInfix t2) $ go b p t2

myConcat :: String -> String -> String
myConcat s "" = s
myConcat "" s = s
myConcat s1 s2 = s1 ++ " " ++ s2

isNotFullAplliedInfix :: LamTerm i Name -> Bool
isNotFullAplliedInfix (Appl _ (Appl _ t1 _ ) _) = not (isInfix t1)
isNotFullAplliedInfix (Appl _ t1 _) = isInfix t1
isNotFullAplliedInfix t = isInfix t

isInfix :: LamTerm i Name -> Bool
isInfix (Val _ BuildIn {fixity = InFix {}}) = True
isInfix _ = False

acumulateVars :: LamTerm i Name -> ([String], LamTerm i Name)
acumulateVars = go []
 where go ns (Lambda _ (Name n) t ) = go (n : ns) t
       go ns t = (reverse ns, t)
