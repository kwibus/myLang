module ExampleLambda where

import MakeTerm

import Name
import Lambda

id :: String -> LamTerm () Name
id name = lambda name $ var name

s :: LamTerm () Name
s = lambda "x" (lambda "y" (var "x"))
