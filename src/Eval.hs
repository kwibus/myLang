module Eval (
  eval
  , fullEval
  , applyValue
  , evalWithEnv)
where

import Control.Monad.State.Strict
import Data.DList
import Data.Maybe
import Data.Bifunctor
import Data.List (foldl')

import Lambda
import BruijnTerm
import Value
import BruijnEnvironment
import Type

-- TODO write Test  for correct order
-- |eval term in accordance with call by value.
-- If a term can't be further be evaluated it will return 'Nothing'
eval :: Show i => BruijnTerm i -> Maybe (BruijnTerm i)
eval = fmap fst . listToMaybe . toList . evalWithEnv bEmtyEnv

evalWithEnv :: Show i => BruijnEnv (BruijnTerm i) -> BruijnTerm i ->
  DList (BruijnTerm i, BruijnEnv (BruijnTerm i))
evalWithEnv env (Appl i func args) = (firstFullExpr `append` nextFullExpr ) `append` final
  where
    evalFunc = evalWithEnv env func
    firstFullExpr = first (\ t -> Appl i t args) <$> evalFunc
    valueFunc = saveLast ( fst <$> toList evalFunc ) func

    evalArgs = evalWithEnv env args
    nextFullExpr = first (Appl i valueFunc) <$> evalArgs
    valueArgs = saveLast (fst <$> toList evalArgs) args

    final = case valueFunc of
      (Lambda _ _ t1) ->
            let step = substitute valueArgs (Bound 0) t1 -- reduce outer to inner redex
            in cons (step, env) $ evalWithEnv env step
      (Val i1 v1) -> return (Val i1 $ applyValue v1 $ value valueArgs, env)
      _ -> empty

evalWithEnv env (Let i defs term) = snoc firstSteps (saveLast (toList evals) (term, env))
  where
    firstSteps = fmap (\ (newTerm, newEnv) -> (Let i (updateDefs newEnv) newTerm, newEnv)) evals
    resetDepth newEnv = newEnv -- newEnv {bruijnDepth = bruijnDepth env}
    updateDefs newEnv = zipWith
      (\ (Def info n _) index -> Def info n ( bLookup index (resetDepth newEnv) ))
      defs
      (Bound <$> [length defs - 1 .. 0])
    evals = evalWithEnv (foldl' (\ envN (Def _ _ tn ) -> bInsert tn envN ) env defs ) term

evalWithEnv env (Var i b) =
  if isvalue valueOfB
  then singleton (valueOfB, env)
  else let evaluntilValue = evalWithEnv env valueOfB
           resetResult = fmap $ first $ const (Var i b)
           lastStep = (\ (result, newEnv) -> (result, bReplace b result newEnv )) $
                      last $ toList evaluntilValue
      in snoc (resetResult evaluntilValue) lastStep
  where valueOfB = bLookup b env
evalWithEnv _ _ = empty

saveLast :: [a] -> a -> a
saveLast [] a = a
saveLast xs _ = last xs

value :: Show i => BruijnTerm i -> Value
value (Val _ v ) = v
value t = error $ show t ++ " is not a value"

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
isvalue Let {} = False

fullEval :: Show i => BruijnTerm i -> BruijnTerm i
fullEval t = saveLast (fst <$> toList ( evalWithEnv bEmtyEnv t )) t
