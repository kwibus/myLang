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

presAplicationfuction :: (Precedence, Associativity)
presAplicationfuction = (11, AssoLeft)

presAplicationfuctionl :: (Precedence, Associativity)
presAplicationfuctionl = (10, AssoLeft)

lowPres :: (Precedence, Associativity)
lowPres = (0, AssoLeft)

highPres :: (Precedence, Associativity)
highPres = (100, AssoRight)

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
  go b p (Lambda _ (Name n) t) = parensIf b $
     "\\" ++ n ++ "." ++ go b p t

  go b p (Appl _ t1 t2 ) = parensIf ( lowerPres (getPresFuntion t1) p && not (isInfix t1)) $
        let string2 = case t2 of
                    (Var _ (Name "#")) -> ""
                    _ -> parensIf (isNotFullAplliedInfix t2) $ go (isInfix t1 || b) (getPresFuntion t1) t2
            string1 = go True presAplicationfuction t1
        in if isInfix t1
           then myConcat string2 string1
           else myConcat string1 string2

myConcat :: String -> String -> String
myConcat "" s = s
myConcat s1 s2 = s1 ++ " " ++ s2

isNotFullAplliedInfix :: LamTerm i Name -> Bool
isNotFullAplliedInfix (Appl _ (Appl _ t1 _ ) _) = not (isInfix t1)
isNotFullAplliedInfix (Appl _ t1 _) = isInfix t1
isNotFullAplliedInfix t = isInfix t

isInfix :: LamTerm i Name -> Bool
isInfix (Val _ BuildIn {fixity = InFix {}}) = True
isInfix _ = False
