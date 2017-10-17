module MakeAnormal where --TODO capital N

import ANormalForm
import Value (Primative(..))
import qualified Operator
import BruijnTerm (Bound(..),Name)

val :: Value -> ANorm
val = Val

bvar :: Int -> Value
bvar = Var . Bound

double :: Double -> Value
double = Constant . MyDouble

true :: Value
true = Constant $ MyBool True

false:: Value
false = Constant $ MyBool False

plus :: Value
plus = Instruc Operator.plus

multiply :: Value
multiply = Instruc Operator.multiply

appl :: Value -> [Value] -> ANorm
appl = Appl

-- TODO consider String
lambda :: [Name] -> ANorm -> Value
lambda [] _ = error "can create a lambda without names"
lambda [n] t = Lambda n t
lambda (n:names) t =  Lambda n $ aLambda
  where
   aLambda = foldr (\name body -> val $ Lambda name body) t names

mkLet :: [(Maybe Name,ANorm)] -> ANorm -> ANorm
mkLet defs t = Let (map (uncurry Def) defs) t
