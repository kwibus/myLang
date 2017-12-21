module ExampleBruijn where

import MakeTerm

import BruijnTerm

omega :: BruijnTerm () ()
omega = lambda "a" $ appl (bvar 0) (bvar 0)

id :: BruijnTerm () ()
id = lambda "a" $ bvar 0

pair :: BruijnTerm () ()
pair = lambda "f" $ lambda "s" $ lambda "b" $ appl (appl (bvar 0) (bvar 2)) (bvar 1)

k :: BruijnTerm () ()
k = lambda "x" (lambda "y" (bvar 1))
