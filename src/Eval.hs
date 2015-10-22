module Eval (eval, fullEval, applyValue)
where

import Control.Monad.State.Strict
import Control.Arrow

import Lambda
import BruijnTerm
import Value
import BruijnEnvironment
import Type

-- TODO write Test  for correct order
-- |eval term in accordance with call by value.
-- If a term can't be further be evaluated it will return 'Nothing'
eval :: BruijnTerm i -> Maybe (BruijnTerm i)
eval = fmap snd . evalWithEnv bEmtyEnv

evalWithEnv :: BruijnEnv (BruijnTerm i) -> BruijnTerm i -> Maybe (BruijnEnv (BruijnTerm i), BruijnTerm i)
evalWithEnv env (Appl i t1 t2)
    | isvalue t2 = case t1 of -- only reduce t1 if t2 is full reduced
        (Lambda _ _ t11) -> Just (env, substitute t2 (Bound 0) t11) -- reduce outer to inner redex
        (Val i1 v1) -> Just (env, Val i1 $ applyValue v1 $ value t2)
        (t11@Appl {}) -> (\ (_, t) -> (env, Appl i t t2 )) <$> evalWithEnv env t11
        _ -> Nothing
    | otherwise = second (Appl i t1) <$> evalWithEnv env t2
evalWithEnv _ _ = Nothing

value :: BruijnTerm i -> Value
value (Val _ v ) = v
value _ = error "type error value, is not a value"

-- | applys a build in function to one argument
--  It crashes if first argument is not a function (It only export to include int test)
applyValue :: Value -- ^ build in function
  -> Value -- ^ argument
  -> Value -- ^ result
applyValue BuildIn {arrity = 1, evaluator = e, stack = s } v = evalState e (v : s )
applyValue v1@BuildIn {arrity = n, stack = s, myType = t } v2 =
    v1 {arrity = n - 1 , stack = v2 : s, myType = dropTypeArg t}
applyValue _ _ = error "apply value"

-- TODO remove initial index
substitute :: BruijnTerm i -> Bound -> BruijnTerm i -> BruijnTerm i
substitute t1 n1 t2@(Var _ n2) = if n1 == n2 then t1 else t2
substitute t1 (Bound i1) (Lambda i n t2) = Lambda i n $
                    substitute t1 (Bound (i1 + 1)) t2
substitute t n (Appl i t1 t2) = Appl i (substitute t n t1) (substitute t n t2)
substitute _ _ t2 = t2

isvalue :: LamTerm i n -> Bool
isvalue Var {} = False --TODO check
isvalue Val {} = True
isvalue Appl {} = False
isvalue Lambda {} = True


-- | fully evaluate a term in accordance with call by value.
fullEval :: BruijnTerm i -> BruijnTerm i
fullEval = go bEmtyEnv
 where
   go env t = case evalWithEnv env t of
    Nothing -> t
    Just r -> uncurry go r
