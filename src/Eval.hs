module Eval (eval, fullEval, apply)
where

import Control.Monad.State.Strict

import Lambda
import BruijnTerm
import Value
import Environment
import Type

eval :: BruijnTerm i -> Maybe (BruijnTerm i)
eval (Lambda {}) = Nothing
eval (Appl i t1 t2)
    | isvalue t2 = case t1 of
        (Lambda _ _ t11) -> Just $ substitute t2 (Bound 0) t11
        (Val i1 (t11@BuildIn {})) -> Just $ Val i1 $ apply t11 $ value t2
        (t11@Appl {}) -> (\ t -> Appl i t t2 ) <$> eval t11
        _ -> Nothing
    | otherwise = Appl i t1 <$> eval t2
eval (_) = Nothing

value :: BruijnTerm i -> Value
value (Val _ v ) = v
value _ = error "type error value, is not a value"

apply :: Value -> Value -> Value
apply BuildIn {arrity = 1, evaluator = e, stack = s } v = evalState e (v : s )
apply v1@BuildIn {arrity = n, stack = s, myType = t } v2 =
    v1 {arrity = n - 1 , stack = v2 : s, myType = tDrop t}
apply _ _ = error "apply value"

tDrop :: Type v -> Type v
tDrop (TAppl _ t ) = t
tDrop _ = error "apply non function"

-- Todo remove inita index
substitute :: BruijnTerm i -> Bound -> BruijnTerm i -> BruijnTerm i
substitute t1 n1 t2@(Var _ n2) = if n1 == n2 then t1 else t2
substitute t1 (Bound i1) (Lambda i n t2) = Lambda i n $
                    substitute t1 (Bound (i1 + 1)) t2
substitute t n (Appl i t1 t2) = Appl i (substitute t n t1) (substitute t n t2)
substitute _ _ t2 = t2

isvalue :: LamTerm i n -> Bool
isvalue Var {} = True
isvalue Val {} = True
isvalue Appl {} = False
isvalue Lambda {} = True

fullEval :: BruijnTerm i -> BruijnTerm i
fullEval t = case eval t of
    Nothing -> t
    Just r -> fullEval r
