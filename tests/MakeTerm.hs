module MakeTerm where

import BruijnTerm
import Value
import Name

val :: Value -> LamTerm () () n
val = Val ()

var :: String -> LamTerm () () Name
var = Var () . fromString

double :: Double -> LamTerm () () n
double = Val () . Prim . MyDouble

true :: LamTerm () () n
true = Val () $ Prim $ MyBool True

false :: LamTerm () () n
false = Val () $ Prim $ MyBool False

bvar :: Int -> BruijnTerm () ()
bvar = Var () . Bound

lambda :: String -> LamTerm () () n -> LamTerm () () n
lambda = Lambda () . fromString

appl :: LamTerm ()() n -> LamTerm () () n -> LamTerm () () n
appl = Appl

mkLet :: [(String, LamTerm () () n)] -> LamTerm () () n -> LamTerm () () n
mkLet tupleDef = Let () (map (uncurry (Def (). Name )) tupleDef)

def :: String -> t -> Def () t
def = Def () . Name
