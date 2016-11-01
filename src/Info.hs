module Info
    ( module Info
    , SourcePos
    , sourceLine
    , sourceName
    , sourceColumn
    ) where

import Lambda
import Name
import Text.Parsec.Pos

type Expresion = LamTerm SourcePos Name

getPosition :: LamTerm SourcePos n -> SourcePos
getPosition (Var i _ ) = i
getPosition (Appl e _) = getPosition e
getPosition (Lambda i _ _) = i
getPosition (Let i _ _ ) = i
getPosition (Val i _) = i

getLastWordPos :: LamTerm SourcePos n -> SourcePos
getLastWordPos (Appl _ e) = getLastWordPos e
getLastWordPos (Lambda _ _ e) = getLastWordPos e
getLastWordPos (Let _ _ e) = getLastWordPos e
getLastWordPos e = getPosition e

removeInfo :: LamTerm i n -> LamTerm () n
removeInfo (Appl e1 e2) = Appl (removeInfo e1) (removeInfo e2)
removeInfo (Val _ v) = Val () v
removeInfo (Lambda _ n e) = Lambda () n $ removeInfo e
removeInfo (Var _ n) = Var () n
removeInfo (Let _ defs e) = Let () (map removeInfoDef defs) $ removeInfo e
  where removeInfoDef (Def _ n e1) = Def () n $ removeInfo e1

showPosition :: SourcePos -> String
showPosition pos = showfile ++
              show ( sourceLine pos) ++ ":" ++
              show ( sourceColumn pos) ++ ":"
  where showfile = if sourceName pos /= ""
        then sourceName pos ++ ":"
        else ""
