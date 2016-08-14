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

class HasPostion a where
    getPosition :: a -> SourcePos

instance HasPostion SourcePos where
    getPosition = id

instance (HasPostion i , HasPostion v) => HasPostion (LamTerm v i n) where
    getPosition (Var i _ ) = getPosition i
    getPosition (Appl e _) = getPosition e
    getPosition (Lambda i _) = getPosition i
    getPosition (Let i _ _ ) = getPosition i
    getPosition (Lit i _) = getPosition i

getLastWordPos :: HasPostion i => LamTerm loc i n -> SourcePos
getLastWordPos (Appl _ e) = getLastWordPos e
getLastWordPos (Lambda _ e) = getLastWordPos e
getLastWordPos (Let _ _ e) = getLastWordPos e
getLastWordPos (Var i _) = getPosition i
getLastWordPos (Lit i _) = getPosition i

removeInfo :: HasName v => LamTerm v i n -> LamTerm Name () n
removeInfo (Appl e1 e2) = Appl (removeInfo e1) (removeInfo e2)
removeInfo (Lit _ v) = Lit () v
removeInfo (Lambda v e) = Lambda  (getName v)$ removeInfo e
removeInfo (Var _ n) = Var () n
removeInfo (Let _ defs e) = Let () (map removeInfoDef defs) $ removeInfo e
  where removeInfoDef (Def n e1) = Def (getName n) $ removeInfo e1

showPosition :: SourcePos -> String
showPosition pos = showfile ++
              show ( sourceLine pos) ++ ":" ++
              show ( sourceColumn pos) ++ ":"
  where showfile = if sourceName pos /= ""
        then sourceName pos ++ ":"
        else ""
