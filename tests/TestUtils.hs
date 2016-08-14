module TestUtils where

import Lambda
import BruijnTerm
import BruijnEnvironment
import FreeEnvironment
import Type
import Name

normalised :: (HasName v, Eq i, Eq v) => LamTerm v i Bound -> Bool
normalised t = fmap lam2Bruijn (bruijn2Lam t) == return ( return t)

welFormd :: LamTerm v i Bound -> Bool
welFormd t0 = go t0 0
    where go (Lambda _ t) dept = go t (dept + 1)
          go (Appl t1 t2) dept = go t1 dept && go t2 dept
          go (Var _ (Bound i) ) dept = i < dept && i >= 0
          go Lit {} _ = True

-- TODO uneeded check
welFormdType :: Type -> Bool
welFormdType = go
    where go (TAppl t1 t2) = go t1 && go t2
          go (TVar (Free i) ) = i >= 0
          go (TPoly(Free i) ) = i >= 0
          go TVal {} = True

size :: LamTerm v a i -> Int
size (Lambda _ e ) = size e + 1
size (Appl e1 e2) = size e1 + size e2
size _ = 1
