module TestUtils where

import Vallue
import Lambda
import BruijnTerm
import Enviroment
import Type

welFormd :: BruijnTerm Vallue -> Bool
welFormd t0 = go t0 0
    where go (Lambda _ t) dept = go t (dept + 1)
          go (Appl t1 t2) dept = go t1 dept && go t2 dept
          go (Var (Bound i) ) dept = i < dept
          go (Val {}) _ = True

-- TODO uneeded check
welFormdType :: Type Bound -> Bool
welFormdType t0 = go t0
    where go (TAppl t1 t2) = go t1 && go t2
          go (TVar (Bound i) ) = i >= 0
          go (TVal {}) = True
