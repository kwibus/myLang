module ExampleBruijn where

import BruijnTerm

omega :: BruijnTerm
omega = BLambda "a" $ BAppl (Bound 0) (Bound 0)

id :: BruijnTerm
id = BLambda "a" $ Bound 0

s :: BruijnTerm
s = BLambda "x" (BLambda "y" (Bound 1))
