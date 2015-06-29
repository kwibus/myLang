module Expresion where

import Vallue
import Lambda
import Names
import Info
type Expresion = LamTerm Loc Name

pShow :: LamTerm i Name -> String
pShow = go False where
      go _ (Var _ (Name n)) = n
      go _ (Val _ v) = pShowVal v
      go b (Lambda _ (Name "#") t) = go b t
      go b (Lambda _ (Name n) t) = "\\" ++ n ++ "." ++ go b t
      go b (Appl _ t1@Lambda {} t2@Var {}) = parentheses t1 ++ go b t2
      go _ (Appl _ t1@Lambda {} t2) = parentheses t1 ++ parentheses t2
      go b (Appl _ t1@Var {} t2@Var {}) = go b t1 ++ " " ++ go b t2
      go b (Appl _ t1@Var {} t2) = go b t1 ++ parentheses t2
      go b (Appl _ t1@(Val _ v1) t2@Val {}) = if isInfixVallue v1
                                        then go b t2 ++ " " ++ go b t1
                                        else go b t1 ++ " " ++ go b t2
      go b (Appl _ t1@(Val _ v) t2) = if isInfixVallue v
                                 then case t2 of
                                     Var _ (Name "#") -> go b t1
                                     _ -> parentheses t2 ++ go b t1
                                 else go b t1 ++ parentheses t2
      go _ (Appl _ t1@Appl {} t2@Appl {}) = go True t1 ++ parentheses t2
      go True (Appl _ t1@Appl {} t2@Lambda {}) = go True t1 ++ parentheses t2
      go b (Appl _ t1@Appl {} t2@Var {}) = go True t1 ++ " " ++ go b t2
      go b (Appl _ t1@Appl {} t2@(Val _ v)) = go True t1 ++ if isInfixVallue v
                                                     then parentheses t2
                                                     else " " ++ go b t2
      go b (Appl _ t1@Appl {} t2 ) = go True t1 ++ go b t2

parentheses :: LamTerm i Name -> String
parentheses s = "(" ++ pShow s ++ ")"