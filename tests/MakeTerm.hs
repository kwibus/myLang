module MakeTerm where

import BruijnTerm
import Lambda
import Value
import BruijnEnvironment
import Name

val :: Value -> LamTerm () n
val = Val ()

var :: String -> LamTerm () Name
var = Var () . Name

double :: Double -> LamTerm () n
double = Val () . MyDouble

bvar :: Int -> BruijnTerm ()
bvar = Var () . Bound

lambda :: String -> LamTerm () n -> LamTerm () n
lambda = Lambda () . Name

appl :: LamTerm () n -> LamTerm () n -> LamTerm () n
appl = Appl ()
