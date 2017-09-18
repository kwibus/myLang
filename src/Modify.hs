module Modify where

import MTable
import BruijnTerm hiding (incFree)
import LambdaF

-- TODO switch result around peek
--
-- | peek will apply modifications stored in mtabel and apply it on first node in ast
-- and will give back a new 'MTable' that can be used to view the subtrees
--
-- you should only use the new 'MTable' on the found subtrees because:
--
-- * 'MTable' keep track of depth of the subtree,
--
-- * if a variable is 'substituted' for new subtree, then that subtree need to be ajusted
--   mtable keep track of this.
--

peek :: MTable -> BruijnTerm ()-> (LamTermF () Bound (BruijnTerm ()), MTable)
peek modifications term = case term of
  Val i v -> (ValF i v,modifications)
  Var _ b -> case peekVar modifications b of
    (Left newB) -> (VarF () newB,modifications)
    (Right (t,newM)) -> peek newM t
  Appl t1 t2 -> (ApplF t1 t2,modifications)
  Lambda i n t -> (LambdaF i n t,extraSparceInsertUndefind 1 modifications)
  Let i defs t -> (LetF i defs t,extraSparceInsertUndefind (length defs) modifications )

-- | will apply the modifications stored in mtable
proces ::  MTable  -> BruijnTerm ()-> BruijnTerm ()
proces m t = if isNull m
             then t
             else unfold peek m t
