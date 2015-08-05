module ExampleBruijn where

import MakeTerm

import BruijnTerm

omega :: BruijnTerm ()
omega = lambda "a" $ appl (bvar 0) (bvar 0)

id :: BruijnTerm ()
id = lambda "a" $ bvar 0

s :: BruijnTerm ()
s = lambda "x" (lambda "y" (bvar 1))
