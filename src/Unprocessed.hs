module Unprocessed where

import MTable
import qualified ModifiedLambda as Mod
import LambdaF
import BruijnEnvironment (Bound)
import BruijnTerm as Lam (Value,BruijnTerm,Def(..))
import TaggedLambda as Tag
import ModificationTags

data Unprocessed = Un MTable (Mod.LamTerm ()) deriving Show

peek :: Unprocessed -> LamTermF () Bound Unprocessed
peek (Un mtable ast) = fmap (Un newMtable ) astF
  where
       (astF,newMtable) = Mod.peek mtable ast

proces :: Unprocessed -> BruijnTerm ()
proces (Un mtable ast ) = Mod.proces mtable ast

procesDef :: DefF () Bound Unprocessed -> Lam.Def () Bound
procesDef (DefF i b t) = Lam.Def i b $ proces t

reproces :: BruijnTerm () -> Unprocessed
reproces ast = Un empty (Tag.tag ast)

--TODO add val var
val ::  Value -> Unprocessed
val v = Un empty $ Val () v

var :: Bound -> Unprocessed
var b = Un empty  $ Var () b

substitute  :: BruijnTerm () -> Unprocessed -> Unprocessed
substitute sub (Un mtable ast) = Un (MTable.drop 1 mtable) $ Tag.Tag (Substitut 0 sub) ast
