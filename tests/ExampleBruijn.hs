module ExampleBruijn where

import BruijnTerm

omega :: BruijnTerm
omega = BLambda "a" $ BAppl (bvar 0) (bvar 0)

id :: BruijnTerm
id = BLambda "a" $ bvar 0

s :: BruijnTerm
s = BLambda "x" (BLambda "y" (bvar 1))
