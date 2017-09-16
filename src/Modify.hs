module Modify where

import MTable
import BruijnTerm hiding (incFree)
import LambdaF

-- TODO make test

peek :: MTable -> BruijnTerm ()-> (LamTermF () Bound (BruijnTerm ()), MTable)
peek modifications term = case term of
  Val i v -> (ValF i v,modifications)
  Var _ b -> case peekVar modifications b of
    (Left newB,newM) -> (VarF () newB,newM)
    (Right t,newM) -> peek newM t
  Appl t1 t2 -> (ApplF t1 t2,modifications)
  Lambda i n t -> (LambdaF i n t,extraSparceInsertUndefind 1 modifications)
  Let i defs t -> (LetF i defs t,extraSparceInsertUndefind (length defs) modifications )

proces ::  MTable  -> BruijnTerm ()-> BruijnTerm ()
proces = unfold peek
