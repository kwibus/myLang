module Unprocessed where

import MTable
import qualified ModifiedLambda as Mod
import LambdaF
import BruijnEnvironment (Bound,bInsert)
import BruijnTerm as Lam (Value,BruijnTerm)
import TaggedLambda as Tag

data Unprocessed = Un MTable (Mod.LamTerm ()) deriving Show

peek :: Unprocessed -> LamTermF () Bound Unprocessed
peek (Un mtable ast) = fmap (Un newMtable ) astF
  where
       (astF,newMtable) = Mod.peek mtable ast

proces :: Unprocessed -> BruijnTerm ()
proces (Un mtable ast ) = Mod.proces mtable ast

reproces :: BruijnTerm () -> Unprocessed
reproces ast = Un empty (Tag.tag ast)

val ::  Value -> Unprocessed
val v = Un empty $ Val () v

var :: Bound -> Unprocessed
var b = Un empty  $ Var () b

getVal :: Unprocessed -> Maybe Value
getVal (Un _ t) = Tag.getVal t

substitute  ::Int->  BruijnTerm () -> Unprocessed -> Unprocessed
-- TODO roundabout way
substitute n sub (Un mtable ast) =  Un (MTable depth (bInsert (Subst (depth-n) sub) env) )  ast
  where
    (MTable depth env ) = MTable.drop 1 mtable

insertUndefined :: Int -> Unprocessed -> Unprocessed
insertUndefined n (Un m t) = Un (insertT (map Undefined  [depth .. depth+n-1]) m) t
  where
    depth = getDepth m
