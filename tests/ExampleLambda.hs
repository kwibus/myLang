module ExampleLambda where

import MakeTerm

import Name
import Lambda

id :: String -> LamTerm Name () Name
id name = lambda name $ var name

k :: LamTerm Name () Name
k = lambda "x" (lambda "y" (var "x"))
