module MakeTerm where

import BruijnTerm
import Lambda
import Value
import BruijnEnvironment
import Name

val :: Value -> LamTerm () n
val = Val ()

var :: String -> LamTerm () Name
var = Var () . fromString

double :: Double -> LamTerm () n
double = val . Prim . MyDouble

false :: LamTerm () n
false = val . Prim . MyBool $ False

true :: LamTerm () n
true = val . Prim . MyBool $ True

bvar :: Int -> BruijnTerm ()
bvar = Var () . Bound

lambda :: String -> LamTerm () n -> LamTerm () n
lambda = Lambda () . fromString

appl :: LamTerm () n -> LamTerm () n -> LamTerm () n
appl = Appl ()

mkLet :: [(String, LamTerm () n)] -> LamTerm () n -> LamTerm () n
mkLet tupleDef = Let () (map (uncurry (Def (). Name )) tupleDef)
