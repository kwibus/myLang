module MakeTypedTerm where

import Type
import BruijnTerm
import Value
import Name

val :: Value -> LamTerm i () n
val = Val ()

var :: String -> LamTerm i () Name
var = Var () . fromString

double :: Double -> LamTerm i () n
double = Val () . Prim . MyDouble

true :: LamTerm i () n
true = Val () $ Prim $ MyBool True

false :: LamTerm i () n
false = Val () $ Prim $ MyBool False

bvar :: Int -> BruijnTerm j ()
bvar = Var () . Bound

lambda :: String -> Type ->  LamTerm Type () n -> LamTerm Type () n
lambda str t e = Lambda t (fromString str) e

appl :: LamTerm i () n -> LamTerm i () n -> LamTerm i () n
appl = Appl

mkLet :: [(String,Type ,LamTerm Type () n)] -> LamTerm Type () n -> LamTerm Type () n
mkLet tupleDef = Let () (map ( \ (str, t, e) -> Def t (fromString str) e ) tupleDef)

def :: String -> Type  -> t -> Def Type t
def str t e = Def t  (fromString str) e
