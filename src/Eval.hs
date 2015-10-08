module Eval (eval, fullEval, apply)
where

import Control.Monad.State.Strict

import Lambda
import BruijnTerm
import Value
import Environment
import Type

-- TODO write Test  for correct order
-- |eval term in accordance with call by value.
-- If a term can't be further be evaluated it will return 'Nothing'
eval :: BruijnTerm i -> Maybe (BruijnTerm i)
eval (Lambda {}) = Nothing
eval (Appl i t1 t2)
    | isvalue t2 = case t1 of -- only reduce t1 if t2 is full reduced
        (Lambda _ _ t11) -> Just $ substitute t2 (Bound 0) t11 -- reduce outer to inner redex
        (Val i1 v1) -> Just $ Val i1 $ apply v1 $ value t2
        (t11@Appl {}) -> (\ t -> Appl i t t2 ) <$> eval t11  
        _ -> Nothing
    | otherwise = Appl i t1 <$> eval t2
eval (_) = Nothing

value :: BruijnTerm i -> Value
value (Val _ v ) = v
value _ = error "type error value, is not a value"


-- | applys a build in function to one argument
--  It crashes if first argument is not a function (It only export to include int test)
apply :: Value -- ^ build in function
  -> Value -- ^ argument
  -> Value -- ^ result
apply BuildIn {arrity = 1, evaluator = e, stack = s } v = evalState e (v : s )
apply v1@BuildIn {arrity = n, stack = s, myType = t } v2 =
    v1 {arrity = n - 1 , stack = v2 : s, myType = dropTypeArg t}
apply _ _ = error "apply value"

-- TODO remove initial index
substitute :: BruijnTerm i -> Bound -> BruijnTerm i -> BruijnTerm i
substitute t1 n1 t2@(Var _ n2) = if n1 == n2 then t1 else t2
substitute t1 (Bound i1) (Lambda i n t2) = Lambda i n $
                    substitute t1 (Bound (i1 + 1)) t2
substitute t n (Appl i t1 t2) = Appl i (substitute t n t1) (substitute t n t2)
substitute _ _ t2 = t2

isvalue :: LamTerm i n -> Bool
isvalue Var {} = False
isvalue Val {} = True
isvalue Appl {} = False
isvalue Lambda {} = True


-- | fully evaluate a term in accordance with call by value.
fullEval :: BruijnTerm i -> BruijnTerm i
fullEval t = case eval t of
    Nothing -> t
    Just r -> fullEval r
