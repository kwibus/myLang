module MakeTerm where

import Lambda
import Value
import BruijnEnvironment
import Name

val :: Value -> LamTerm Name () n
val = Lit ()

var :: String -> LamTerm Name () Name
var = Var () . fromString

double :: Double -> LamTerm Name () n
double = val . Prim . MyDouble

bvar :: Int -> LamTerm Name () Bound
bvar = Var () . Bound

lambda :: String -> LamTerm Name () n -> LamTerm Name () n
lambda = Lambda  . fromString

appl :: LamTerm Name () n -> LamTerm Name () n -> LamTerm Name () n
appl = Appl

mkLet :: [(String, LamTerm Name () n)] -> LamTerm Name () n -> LamTerm Name () n
mkLet tupleDef = Let () (map (uncurry (Def . Name )) tupleDef)
