module ExampleBruijn where

import Vallue
import MakeTerm
omega :: BruijnTerm Vallue
omega = lambda "a" $ appl (bvar 0) (bvar 0)

id :: BruijnTerm Vallue
id = lambda "a" $ bvar 0

s :: BruijnTerm Vallue
s = lambda "x" (lambda "y" (bvar 1))
