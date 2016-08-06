module Info where

import Lambda
import Name

type Expresion = LamTerm Loc Name

data Loc = Loc
    { srcFile :: String
    , lineStart :: Int
    , columnStart :: Int
    , lineEnd :: Int
    , columnEnd :: Int
    } deriving (Show, Eq)

showLoc :: Loc -> String
showLoc loc = showfile ++
              show ( lineStart loc) ++ ":" ++
              show ( columnStart loc) ++ ":"
  where showfile = if srcFile loc /= ""
        then srcFile loc ++ ":"
        else ""

getInfo :: LamTerm i n -> i
getInfo (Var i _ ) = i
getInfo (Appl i _ _) = i
getInfo (Lambda i _ _) = i
getInfo (Let i _ _ ) = i
getInfo (Lit i _) = i

setInfo :: i -> LamTerm i n -> LamTerm i n
setInfo loc (Var _ n ) = Var loc n
setInfo loc (Appl _ e1 e2) = Appl loc e1 e2
setInfo loc (Lambda _ n e) = Lambda loc n e
setInfo loc (Lit _ v) = Lit loc v

getLocation :: LamTerm Loc n -> Loc
getLocation = getInfo

removeInfo :: LamTerm i n -> LamTerm () n
removeInfo (Lambda _ n e) = Lambda () n $ removeInfo e
removeInfo (Appl _ e1 e2) = Appl () (removeInfo e1 ) $ removeInfo e2
removeInfo (Let _ def term) = Let () (map removeInfoDef def) (removeInfo term)
  where removeInfoDef (Def _ n t) = Def () n $ removeInfo t
removeInfo (Lit _ v) = Lit () v
removeInfo (Var _ n) = Var () n

mergLoc :: LamTerm Loc n -> LamTerm Loc n -> Loc
mergLoc e1 e2 = Loc { srcFile = srcFile start
                    , lineStart = lineStart start
                    , columnStart = columnStart start
                    , lineEnd = lineEnd end
                    , columnEnd = columnEnd end}
    where start = getLocation e1
          end = getLocation e2
