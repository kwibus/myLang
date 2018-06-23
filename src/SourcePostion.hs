module SourcePostion
    ( module SourcePostion
    , SourcePos
    , sourceLine
    , sourceName
    , sourceColumn
    ) where

import Lambda
import Text.Parsec.Pos

type Expresion = LamTerm SourcePos SourcePos Name

getLastWordPos :: LamTerm SourcePos SourcePos n -> SourcePos
getLastWordPos (Appl _ e) = getLastWordPos e
getLastWordPos (Lambda _ _ e) = getLastWordPos e
getLastWordPos (Let _ _ e) = getLastWordPos e
getLastWordPos e = getPosition e

getPosition :: LamTerm SourcePos SourcePos n -> SourcePos
getPosition (Var i _ ) = i
getPosition (Appl e _) = getPosition e
getPosition (Lambda i _ _) = i
getPosition (Let i _ _ ) = i
getPosition (Val i _) = i

showPosition :: SourcePos -> String
showPosition pos = showfile ++
              show ( sourceLine pos) ++ ":" ++
              show ( sourceColumn pos) ++ ":"
  where showfile = if sourceName pos /= ""
        then sourceName pos ++ ":"
        else ""
