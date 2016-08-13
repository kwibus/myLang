module ExampleBruijn where

import MakeTerm
import Lambda
import Name
import BruijnEnvironment

type SimpleBruijn  = LamTerm Name () Bound

omega :: SimpleBruijn
omega = lambda "a" $ appl (bvar 0) (bvar 0)

id :: SimpleBruijn
id = lambda "a" $ bvar 0

pair :: SimpleBruijn
pair = lambda "f" $ lambda "s" $ lambda "b" $ appl (appl (bvar 0)(bvar 2)) (bvar 1)

k :: SimpleBruijn
k = lambda "x" (lambda "y" (bvar 1))
