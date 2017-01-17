module Unprocessed where

import TaggedLambda
import ModificationTags
import BruijnEnvironment
import LambdaF
import Value
import Name

--TODO rename

appl :: Unprocessed i -> Unprocessed i -> Unprocessed i
appl (Unprocessed t1) (Unprocessed t2) = Unprocessed $ Appl t1 t2

-- lambda :: i -> Name -> (Unprocessed i) -> (Unprocessed i)
mkLet :: i -> [DefF i Bound (Unprocessed i)] -> Unprocessed i -> Unprocessed i
mkLet i defs (Unprocessed t) = Unprocessed $ Let i ( map unpeekDef defs) t

val :: i -> Value -> Unprocessed i
val i v = Unprocessed $ Val i v

lambda :: i -> Name -> Unprocessed i -> Unprocessed i
lambda i n (Unprocessed t) = Unprocessed $ Lambda i n t
