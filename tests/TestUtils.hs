module TestUtils where

import Lambda
import BruijnTerm
import Enviroment
import Type

welFormd :: BruijnTerm i -> Bool
welFormd t0 = go t0 0
    where go (Lambda _ _ t) dept = go t (dept + 1)
          go (Appl _ t1 t2) dept = go t1 dept && go t2 dept
          go (Var _ (Bound i) ) dept = i < dept
          go (Val {}) _ = True

-- TODO uneeded check
welFormdType :: Type Bound -> Bool
welFormdType t0 = go t0
    where go (TAppl t1 t2) = go t1 && go t2
          go (TVar (Bound i) ) = i >= 0
          go (TVal {}) = True
