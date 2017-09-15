module Modify where

import MTable
import BruijnTerm hiding (incFree)
import LambdaF
import BruijnEnvironment

-- TODO make test
peek :: MTable -> BruijnTerm ()-> (LamTermF () Bound (BruijnTerm ()), MTable)
peek modifications term = case term of
  Val i v -> (ValF i v,modifications)
  Var _ b -> peekVar modifications b
  Appl t1 t2 -> (ApplF t1 t2,modifications)
  Lambda i n t -> (LambdaF i n t,insertT [Undefined depth] modifications)
  Let i defs t -> ( LetF i defs t,insertT (map Undefined [depth .. depth + nDefs-1]) modifications )
    where
      nDefs = length defs
  where
    depth = getDepth modifications

peekVar :: MTable -> Bound -> (LamTermF () Bound (BruijnTerm ()),MTable)
peekVar modifications b@(Bound n) =
  let table = getEnv modifications
  in case bMaybeLookup b table of
    Just (Undefined depthDefined) -> (VarF () $ Bound $ depth- depthDefined - 1,modifications)
    Just (Subst depthDefined t2) ->
        peek (incFree (depth - depthDefined)empty) $ t2
    Nothing -> ( VarF () (Bound $ n + incFreeFromStart modifications),modifications)
  where
    depth = getDepth modifications


proces ::  MTable  -> BruijnTerm ()-> BruijnTerm ()
proces = unfold peek
