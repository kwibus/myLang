module TestUtils where

import BruijnTerm

welFormd :: BruijnTerm -> Bool
welFormd t0 = go t0 0
    where go (BLambda _ t) dept = go t (dept + 1)
          go (BAppl t1 t2) dept = go t1 dept && go t2 dept
          go (Bound i ) dept = i <= dept
          go (BVal {}) _ = True
