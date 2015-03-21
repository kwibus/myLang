module ExampleBruijn where

import BruijnTerm
import MakeTerm
omega :: BruijnTerm ()
omega = lambda "a" $ appl (bvar 0) (bvar 0)

id :: BruijnTerm ()
id = lambda "a" $ bvar 0

s :: BruijnTerm ()
s = lambda "x" (lambda "y" (bvar 1))
