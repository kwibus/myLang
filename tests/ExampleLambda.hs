module ExampleLambda where

import MakeTerm

import Name
import Lambda

id :: String -> LamTerm () Name
id name = lambda name $ var name

k :: LamTerm () Name
k = lambda "x" (lambda "y" (var "x"))
