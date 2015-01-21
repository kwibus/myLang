module TestUtils where

import Vallue
import Lambda
import BruijnTerm

welFormd :: BruijnTerm Vallue -> Bool
welFormd t0 = go t0 0
    where go (Lambda _ t) dept = go t (dept + 1)
          go (Appl t1 t2) dept = go t1 dept && go t2 dept
          go (Var i ) dept = i <= dept
          go (Val {}) _ = True
