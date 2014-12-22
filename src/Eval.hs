module Eval (eval, fullEval)
where
import BruijnTerm
import Vallue
import Control.Monad.State.Strict

eval :: BruijnTerm -> Maybe BruijnTerm
eval (BVar {}) = Nothing
eval (BLambda {}) = Nothing
eval (BAppl t1 t2)
    | isvalue t2 = case t1 of
        (BLambda _ t11) -> Just $ substitute t2 0 t11
        (BVar (BVal (t11@BuildIn {}))) -> Just $ bval $ apply t11 $ vallue t2
        (t11@BAppl {}) -> fmap (\ t -> BAppl t t2 ) $ eval t11
        _ -> Nothing
    | otherwise = fmap (BAppl t1 ) $ eval t2

vallue :: BruijnTerm -> Vallue
vallue (BVar (BVal v )) = v
vallue t = error $ "type error vallue " ++ show t ++ "is not a vallue"

apply :: Vallue -> Vallue -> Vallue
apply BuildIn {arrity = 1, evaluator = e, stack = s } v = evalState e (v : s )
apply t@BuildIn {arrity = n, stack = s } v = t {arrity = n - 1 , stack = v : s}
apply _ _ = error "apply vallue"

-- Todo remove inita index
substitute :: BruijnTerm -> Index -> BruijnTerm -> BruijnTerm
substitute t1 i1 t2@(BVar (Bound i2)) = if i1 == i2 then t1 else t2
substitute t1 i1 (BLambda n t2) = BLambda n $ substitute t1 (i1 + 1) t2
substitute t i (BAppl t1 t2) = BAppl (substitute t i t1) (substitute t i t2)
substitute _ _ t2 = t2

isvalue :: BruijnTerm -> Bool
isvalue BVar {} = True
isvalue BAppl {} = False
isvalue BLambda {} = True


fullEval :: BruijnTerm -> BruijnTerm
fullEval t = case eval t of
    Nothing -> t
    Just r -> fullEval r
