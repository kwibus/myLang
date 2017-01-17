module Unprocessed where

import TaggedLambda
import ModificationTags
import BruijnEnvironment
import LambdaF
import Value
import Name

-- | when you walk over tree you have to know wich part have already been modified and which not
-- TODO rename inprogress
newtype Unprocessed i = Unprocessed (LamTerm i Bound (Modify i)) deriving (Show, Eq)

addTag :: Modify i -> Unprocessed i -> Unprocessed i
addTag m (Unprocessed t) = Unprocessed $ Tag m t

appl :: Unprocessed i -> Unprocessed i -> Unprocessed i
appl (Unprocessed t1) (Unprocessed t2) = Unprocessed $ Appl t1 t2

-- lambda :: i -> Name -> (Unprocessed i) -> (Unprocessed i)
mkLet :: i -> [DefF i Bound (Unprocessed i)] -> Unprocessed i -> Unprocessed i
mkLet i defs (Unprocessed t) = Unprocessed $ Let i ( map unpeekDef defs) t

unpeekDef :: DefF i Bound (Unprocessed i) -> Def i Bound (Modify i)
unpeekDef (DefF i n (Unprocessed t)) = Def i n t

val :: i -> Value -> Unprocessed i
val i v = Unprocessed $ Val i v

lambda :: i -> Name -> Unprocessed i -> Unprocessed i
lambda i n (Unprocessed t) = Unprocessed $ Lambda i n t
