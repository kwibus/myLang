module Eval (eval, fullEval)
where

import Control.Monad.State.Strict
import Lambda
import BruijnTerm
import Vallue

eval :: BruijnTerm Vallue -> Maybe (BruijnTerm Vallue)
eval (Lambda {}) = Nothing
eval (Appl t1 t2)
    | isvalue t2 = case t1 of
        (Lambda _ t11) -> Just $ substitute t2 0 t11
        (Val (t11@BuildIn {})) -> Just $ Val $ apply t11 $ vallue t2
        (t11@Appl {}) -> fmap (\ t -> Appl t t2 ) $ eval t11
        _ -> Nothing
    | otherwise = fmap (Appl t1 ) $ eval t2
eval (_) = Nothing

vallue :: BruijnTerm Vallue -> Vallue
vallue (Val v ) = v
vallue t = error $ "type error vallue " ++ show t ++ "is not a vallue"

apply :: Vallue -> Vallue -> Vallue
apply BuildIn {arrity = 1, evaluator = e, stack = s } v = evalState e (v : s )
apply t@BuildIn {arrity = n, stack = s } v = t {arrity = n - 1 , stack = v : s}
apply _ _ = error "apply vallue"

-- Todo remove inita index
substitute :: BruijnTerm Vallue -> Index -> BruijnTerm Vallue -> BruijnTerm Vallue
substitute t1 i1 t2@(Var i2) = if i1 == i2 then t1 else t2
substitute t1 i1 (Lambda n t2) = Lambda n $ substitute t1 (i1 + 1) t2
substitute t i (Appl t1 t2) = Appl (substitute t i t1) (substitute t i t2)
substitute _ _ t2 = t2

isvalue :: BruijnTerm Vallue -> Bool
isvalue Var {} = True
isvalue Val {} = True
isvalue Appl {} = False
isvalue Lambda {} = True


fullEval :: BruijnTerm Vallue -> BruijnTerm Vallue
fullEval t = case eval t of
    Nothing -> t
    Just r -> fullEval r
