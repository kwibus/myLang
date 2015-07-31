{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Info where
import Lambda

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
getInfo (Val i _) = i

setInfo :: i -> LamTerm i n -> LamTerm i n
setInfo loc (Var _ n ) = Var loc n
setInfo loc (Appl _ e1 e2) = Appl loc e1 e2
setInfo loc (Lambda _ n e) = Lambda loc n e
setInfo loc (Val _ v) = Val loc v

--TODO rename is confusion with parsec GetPosition
getposition :: LamTerm Loc n -> Loc
getposition = getInfo

removeInfo :: LamTerm i n -> LamTerm () n
removeInfo (Lambda _ n e) = Lambda () n $ removeInfo e
removeInfo (Appl _ e1 e2) = Appl () (removeInfo e1 ) $ removeInfo e2
removeInfo (Val _ v) = Val () v
removeInfo (Var _ n) = Var () n

mergLoc :: LamTerm Loc n -> LamTerm Loc n -> Loc
mergLoc e1 e2 = Loc { srcFile = srcFile start
                    , lineStart = lineStart start
                    , columnStart = columnStart start
                    , lineEnd = lineEnd end
                    , columnEnd = columnEnd end}
    where start = getposition e1
          end = getposition e2

class Position a where
    position :: a -> Maybe Loc

instance {-# OVERLAPPABLE  #-} Position a where
    position _ = Nothing

instance Position Loc where
    position a = Just a
