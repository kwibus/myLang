module ExampleLambda where

import Names
import Lambda
import MakeTerm

id ::Name -> LamTerm () Name
id name = lambda name $ var name

s :: LamTerm () Name
s = lambda "x" (lambda "y" (var "x"))
