module MakeTagedTerm where

import TaggedLambda
import Value
import BruijnEnvironment
import Name

val :: Value -> LamTerm () () n t
val = Val ()

var :: String -> LamTerm () () Name t
var = Var () . fromString

double :: Double -> LamTerm () () n t
double = Val () . Prim . MyDouble

true :: LamTerm () () n t
true = Val () $ Prim $ MyBool True

false :: LamTerm () () n t
false = Val () $ Prim $ MyBool False

bvar :: Int -> LamTerm () () Bound t
bvar = Var () . Bound

lambda :: String -> LamTerm () () n t -> LamTerm () ()n t
lambda = Lambda () . fromString

appl :: LamTerm () () n t -> LamTerm () () n t -> LamTerm () () n t
appl = Appl

mkLet :: [(String, LamTerm () () n t)] -> LamTerm () () n t-> LamTerm () () n t
mkLet tupleDef = Let () (map (uncurry (Def (). Name )) tupleDef)
