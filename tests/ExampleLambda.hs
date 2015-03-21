module ExampleLambda where

import Vallue
import Lambda
import MakeTerm

id :: LamTerm Vallue Name
id = lambda "a" $ var "a"

s :: LamTerm Vallue Name
s = lambda "x" (lambda "y" (var "x"))
