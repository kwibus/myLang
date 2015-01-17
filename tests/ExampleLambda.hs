module ExampleLambda where

import Vallue
import Lambda

id :: LamTerm Vallue Name
id = Lambda "a" $ Var "a"

s :: LamTerm Vallue Name
s = Lambda "x" (Lambda "y" (Var "x"))
