module Expresion where

import Vallue
import Lambda

type Expresion = LamTerm Vallue Name

isinfixLam :: LamTerm Vallue i -> Bool
isinfixLam (Val v) = isInfixVallue v
isinfixLam _ = False

pShow :: Expresion -> String
pShow = go False where
      go _ (Var n) = n
      go _ (Val v) = pShowVal v
      go b (Lambda "#" t) = go b t
      go b (Lambda n t) = "\\" ++ n ++ "." ++ go b t
      go b (Appl t1@Lambda {} t2@Var {}) = parentheses t1 ++ go b t2
      go _ (Appl t1@Lambda {} t2) = parentheses t1 ++ parentheses t2
      go b (Appl t1@Var {} t2@Var {}) = go b t1 ++ " " ++ go b t2
      go b (Appl t1@Var {} t2) = go b t1 ++ parentheses t2
      go b (Appl t1@(Val v1) t2@Val {}) = if isInfixVallue v1
                                        then go b t2 ++ " " ++ go b t1
                                        else go b t1 ++ " " ++ go b t2
      go b (Appl t1@(Val v) t2) = if isInfixVallue v
                                 then if t2 == Var "#"
                                        then go b t1
                                        else parentheses t2 ++ go b t1
                                 else go b t1 ++ parentheses t2
      go _ (Appl t1@Appl {} t2@Appl {}) = go True t1 ++ parentheses t2
      go True (Appl t1@Appl {} t2@Lambda {}) = go True t1 ++ parentheses t2
      go b (Appl t1@Appl {} t2@Var {}) = go True t1 ++ " " ++ go b t2
      go b (Appl t1@Appl {} t2@(Val v)) = go True t1 ++ if isInfixVallue v
                                                     then parentheses t2
                                                     else " " ++ go b t2
      go b (Appl t1@Appl {} t2 ) = go True t1 ++ go b t2
parentheses :: Expresion -> String
parentheses s = "(" ++ pShow s ++ ")"
