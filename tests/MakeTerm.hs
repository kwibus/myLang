module MakeTerm where
import BruijnTerm
import Lambda
import Vallue
import Enviroment
import Names

val :: Vallue -> LamTerm () n
val = Val ()

var :: Name -> LamTerm () Name
var = Var ()

double :: Double -> LamTerm () n
double = (Val ()) . MyDouble

bvar :: Int -> BruijnTerm ()
bvar = (Var ()) . Bound

lambda :: Name -> LamTerm () n -> LamTerm () n
lambda = Lambda ()

appl :: LamTerm () n -> LamTerm () n -> LamTerm () n
appl = Appl ()
