module ExampleLambda where

import Names
import Lambda
import MakeTerm

id :: LamTerm () Name
id = lambda "a" $ var "a"

s :: LamTerm () Name
s = lambda "x" (lambda "y" (var "x"))
