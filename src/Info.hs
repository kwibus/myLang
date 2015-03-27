{-# LANGUAGE OverlappingInstances, FlexibleInstances, UndecidableInstances #-}

module Info where
import Text.Parsec.Pos
import Lambda

data Src = File String | Str String
type Loc = SourcePos -- Loc Src Line Column

getInfo :: LamTerm i n -> i
getInfo (Var i _ ) = i
getInfo (Appl i _ _) = i
getInfo (Lambda i _ _) = i
getInfo (Val i _) = i

getposition :: LamTerm Loc n -> Loc
getposition = getInfo

removeInfo :: LamTerm i n -> LamTerm () n
removeInfo (Lambda _ n e) = Lambda () n $ removeInfo e
removeInfo (Appl _ e1 e2) = Appl () (removeInfo e1 ) $ removeInfo e2
removeInfo (Val _ v) = Val () v
removeInfo (Var _ n) = Var () n

class Position a where
    position :: a -> Maybe Loc

instance Position a where
    position _ = Nothing

instance Position Loc where
    position a = Just a
