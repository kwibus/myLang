module ExampleLambda where

import Lambda

id :: LamTerm
id = Lambda "a" $ var "a"

s :: LamTerm
s = Lambda "x" (Lambda "y" (var "x"))
