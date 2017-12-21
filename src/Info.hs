module Info  where

import Lambda

removeInfo :: LamTerm i n -> LamTerm () n
removeInfo (Appl e1 e2) = Appl (removeInfo e1) (removeInfo e2)
removeInfo (Val _ v) = Val () v
removeInfo (Lambda _ n e) = Lambda () n $ removeInfo e
removeInfo (Var _ n) = Var () n
removeInfo (Let _ defs e) = Let () (map removeInfoDef defs) $ removeInfo e
  where removeInfoDef (Def _ n e1) = Def () n $ removeInfo e1
