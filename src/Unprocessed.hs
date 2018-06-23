module Unprocessed where

import MTable
import qualified Modify as Mod
import LambdaF
import BruijnTerm
data Unprocessed = Un MTable (BruijnTerm() () ) deriving Show

peek :: Unprocessed -> LamTermF () () Bound Unprocessed
peek (Un mtable ast) = fmap (Un newMtable ) astF
  where
       (astF,newMtable) = Mod.peek mtable ast

proces :: Unprocessed -> BruijnTerm () ()
proces (Un mtable ast ) = Mod.proces mtable ast

--TODO this function will will make getDepth no longer accurate
reproces :: BruijnTerm () () -> Unprocessed
reproces ast = Un empty ast

val ::  Value -> Unprocessed
val v = Un empty $ Val () v

var :: Bound -> Unprocessed
var b = Un empty  $ Var () b

incFree :: Int -> Unprocessed -> Unprocessed
incFree n (Un mtable ast) = Un (MTable.incFree n mtable) ast

substitute :: Int->  BruijnTerm () () -> Unprocessed -> Unprocessed
substitute n sub (Un mtable ast) =  Un (MTable.substitute (Bound 0) n sub $ MTable.drop 1 mtable) ast

insertUndefined :: Int -> Unprocessed -> Unprocessed
insertUndefined n (Un m t) = Un (MTable.insertUndefined n m) t
