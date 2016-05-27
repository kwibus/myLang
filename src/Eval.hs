module Eval (
  eval
  , evalSteps
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
eval = listToMaybe . evalSteps

evalSteps ::Show i => BruijnTerm i -> [BruijnTerm i]
evalSteps = fmap fst . toList . evalWithEnv bEmtyEnv

--TODO use Env with Lambda
--TODO use fix Env when free variable
evalWithEnv :: Show i => BruijnEnv (BruijnTerm i) -> BruijnTerm i ->
  DList (BruijnTerm i, BruijnEnv (BruijnTerm i))
evalWithEnv env (Appl func args) = (firstFullExpr `append` nextFullExpr ) `append` final
  where
    evalFunc = evalWithEnv env func
    firstFullExpr = first (\ t -> Appl t args) <$> evalFunc
    valueFunc = saveLast ( fst <$> toList evalFunc ) func

    evalArgs = evalWithEnv env args
    nextFullExpr = first (Appl valueFunc) <$> evalArgs
    valueArgs = saveLast (fst <$> toList evalArgs) args

    final = case valueFunc of
      (Lambda _ _ t1) ->
            let step = substitute valueArgs 0 t1 -- reduce outer to inner redex
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
  case bMaybeLookup b env of
    Just v | isvalue v ->singleton (v, env)
           | otherwise ->
            let evaluntilValue = evalWithEnv env v
                resetResult = fmap $ first $ const (Var i b)
                lastStep = (\ (result, newEnv) -> (result, bReplace b result newEnv )) $
                              last $ toList evaluntilValue
            in snoc (resetResult evaluntilValue) lastStep
    Nothing -> empty
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
substitute :: BruijnTerm i -> Int-> BruijnTerm i -> BruijnTerm i
substitute t1 n1 t2@(Var i (Bound n2)) | n1 == n2 = inc n1 t1
                               | n1 < n2 = Var i $ Bound (n2-1)
                               | otherwise = t2
substitute t1 n1 (Lambda i n2 t2) = Lambda i n2 $
                    substitute t1 (n1 + 1) t2
substitute t n (Appl t1 t2) = Appl (substitute t n t1) (substitute t n t2)
substitute _ _ t2 = t2

inc :: Int -> BruijnTerm i -> BruijnTerm i
inc 0 term = term
inc increase  term = go 0 term
  where go depth (Lambda i n t) = Lambda i n $ go (depth+1) t
        go depth (Appl t1 t2) = Appl (go depth t1)(go depth t2)
        go depth (Var i (Bound n)) | n >= depth  = Var i $ Bound $ n+increase
                                   | otherwise =Var i (Bound n)
        go depth (Let i defs t) = Let i (fmap incDefs defs) $ go newDepth t
          where
            newDepth = depth +length defs
            incDefs (Def is ns ts) = Def is ns $ go newDepth ts
            
        go _ (Val i v) = Val i v

isvalue :: LamTerm i n -> Bool
isvalue Var {} = False --TODO check
isvalue Val {} = True
isvalue Appl {} = False
isvalue Lambda {} = True
isvalue Let {} = False

fullEval :: Show i => BruijnTerm i -> BruijnTerm i
fullEval t = saveLast (fst <$> toList ( evalWithEnv bEmtyEnv t )) t
