module PrettyPrint where

import Value hiding (isInfix)
import Lambda
import Name
import Associativity

getPrec :: LamTerm i n -> (Precedence, Associativity)
getPrec (Val _ BuildIn {fixity = Infix p a}) = (p, a)
getPrec _ = highPrec

getPrecFuntion :: LamTerm i n -> (Precedence, Associativity)
getPrecFuntion (Appl _ (Appl {}) _) = highPrec
getPrecFuntion (Appl _ term _ ) = getPrec term
getPrecFuntion term = getPrec term

decrement :: (Precedence , Associativity) -> (Precedence , Associativity)
decrement (p, AssoLeft ) = (p, AssoRight)
decrement (p, AssoRight) = (p - 1, AssoLeft)

presAplicationfuction :: (Precedence, Associativity)
presAplicationfuction = (11, AssoLeft)

lowPrec :: (Precedence, Associativity)
lowPrec = (0, AssoLeft)

highPrec :: (Precedence, Associativity)
highPrec = (100, AssoLeft)

parensIf :: Bool -> String -> String
parensIf True string = parens string
parensIf False string = string

parens :: String -> String
parens string = "(" ++ string ++ ")"

pShow :: LamTerm i Name -> String
pShow = go False lowPrec
 where
  go _ _ (Var _ (Name n)) = n
  go _ _ (Val _ v) = pShowVal v
  go b _ (Lambda _ (Name "#") t) = go b lowPrec t
  go b p (Lambda _ (Name n) t) =
    let (vars, nextTerm) = accumulateVars t
    in parensIf b $ "\\" ++ unwords (filter (/= "#") (n : vars)) ++ "." ++ go b p nextTerm

  go b p (Appl _ t1 t2 )
    | isInfix t1 =
        let string2 = case t2 of
               (Var _ (Name "#")) -> ""
               _ -> mkString2 True (decrement (getPrecFuntion t1))
        in myConcat string2 string1

    | otherwise = parensIf ( higherPrec p (getPrecFuntion t1)) $
        let string2 = mkString2 b (getPrecFuntion t1)
        in myConcat string1 string2

    where string1 = go True presAplicationfuction t1
          mkString2 b' p' = parensIf (isNotFullAplliedInfix t2) $ go b' p' t2

myConcat :: String -> String -> String
myConcat s "" = s
myConcat "" s = s
myConcat s1 s2 = s1 ++ " " ++ s2

isNotFullAplliedInfix :: LamTerm i Name -> Bool
isNotFullAplliedInfix (Appl _ (Appl _ t1 _ ) _) = not (isInfix t1)
isNotFullAplliedInfix (Appl _ t1 _) = isInfix t1
isNotFullAplliedInfix t = isInfix t


accumulateVars :: LamTerm i Name -> ([String], LamTerm i Name)
accumulateVars = go []
 where go ns (Lambda _ (Name n) t ) = go (n : ns) t
       go ns t = (reverse ns, t)
