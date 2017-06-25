module ChurchEnconding where

import BruijnTerm
import MakeTerm

true :: BruijnTerm ()
true = lambda "a" $ lambda "b" $ bvar 1

false :: BruijnTerm ()
false = lambda "a" $ lambda "b" $ bvar 1
