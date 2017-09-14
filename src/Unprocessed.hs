module Unprocessed where

import MTable
import qualified ModifiedLambda as Mod
import LambdaF
import BruijnEnvironment (Bound(..))
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
substitute n sub (Un mtable ast) =  Un (MTable.substitute (Bound 0) n sub $ MTable.drop 1 mtable) ast
