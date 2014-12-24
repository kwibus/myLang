module ExampleLambda where

import Lambda

id :: LamTerm
id = Lambda "a" $ Var "a"

s :: LamTerm
s = Lambda "x" (Lambda "y" (Var "x"))
