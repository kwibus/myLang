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
import Value
import BruijnEnvironment
import Type

-- TODO write Test  for correct order
-- |eval term in accordance with call by value.
-- If a term can't be further be evaluated it will return 'Nothing'
eval :: (Show v, Show i) => LamTerm v i Bound -> Maybe (LamTerm v i Bound)
eval = fmap fst . listToMaybe . toList . evalWithEnv bEmtyEnv

evalWithEnv :: (Show v, Show i) => BruijnEnv (LamTerm v i Bound) -> LamTerm v i Bound ->
  DList (LamTerm v i Bound , BruijnEnv (LamTerm v i Bound))
evalWithEnv env (Appl func args) = (firstFullExpr `append` nextFullExpr ) `append` final
  where
    evalFunc = evalWithEnv env func
    firstFullExpr = first (\ t -> Appl t args) <$> evalFunc
    valueFunc = saveLast ( fst <$> toList evalFunc ) func

    evalArgs = evalWithEnv env args
    nextFullExpr = first (Appl valueFunc) <$> evalArgs
    valueArgs = saveLast (fst <$> toList evalArgs) args

    final = case valueFunc of
      (Lambda _ t1) ->
            let step = substitute valueArgs (Bound 0) t1 -- reduce outer to inner redex
            in cons (step, env) $ evalWithEnv env step
      (Lit i1 v1) -> return (Lit i1 $ applyValue v1 $ value valueArgs, env)
      _ -> empty

evalWithEnv env (Let i defs term) = snoc firstSteps (saveLast (toList evals) (term, env))
  where
    firstSteps = fmap (\ (newTerm, newEnv) -> (Let i (updateDefs newEnv) newTerm, newEnv)) evals
    resetDepth newEnv = newEnv -- newEnv {bruijnDepth = bruijnDepth env}
    updateDefs newEnv = zipWith
      (\ (Def v _) index -> Def v ( bLookup index (resetDepth newEnv) ))
      defs
      (Bound <$> [length defs - 1 .. 0])
    evals = evalWithEnv (foldl' (\ envN (Def _ tn ) -> bInsert tn envN ) env defs ) term

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

value :: (Show v ,Show i) => LamTerm v i Bound -> Value
value (Lit _ v ) = v
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
substitute :: LamTerm v i Bound -> Bound -> LamTerm v i Bound -> LamTerm v i Bound
substitute t1 n1 t2@(Var _ n2) = if n1 == n2 then t1 else t2
substitute t1 (Bound i1) (Lambda n t2) = Lambda n $
                    substitute t1 (Bound (i1 + 1)) t2
substitute t n (Appl t1 t2) = Appl (substitute t n t1) (substitute t n t2)
substitute _ _ t2 = t2

isvalue :: LamTerm v i n -> Bool
isvalue Var {} = False --TODO check
isvalue Lit {} = True
isvalue Appl {} = False
isvalue Lambda {} = True
isvalue Let {} = False

fullEval :: (Show v, Show i) => LamTerm v i Bound -> LamTerm v i Bound
fullEval t = saveLast (fst <$> toList ( evalWithEnv bEmtyEnv t )) t
