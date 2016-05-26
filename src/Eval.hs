{-# LANGUAGE TupleSections #-}
module Eval (
  substituteEnv
  , updateEnv
  , eval
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
--TODO make result  (DList,end ) ore other thrick to nut use last
--TODO fix names
evalWithEnv :: Show i => BruijnEnv (BruijnTerm i) -> BruijnTerm i ->
  DList (BruijnTerm i, BruijnEnv (BruijnTerm i))
evalWithEnv env (Appl func args) = (firstFullExpr `append` nextFullExpr ) `append` final
  where
    evalFunc = evalWithEnv env func
    firstFullExpr = (\(t,envN) -> (Appl t (substituteEnv envN args),env)) <$> evalFunc
    (valueFunc,envfunction) = saveLastD  evalFunc (func,env)
    newerEnv = dropNew env envfunction

    evalArgs = evalWithEnv env args
    nextFullExpr = first (Appl valueFunc) <$> evalArgs
    (valueArgs,envarg) = saveLastD evalArgs (args,env)

    final = case valueFunc of
      (Lambda _ _ t1) -> let newestEnv = bInsert valueArgs env
                         in cons (substituteEnv newestEnv t1,newestEnv) (evalWithEnv newestEnv  t1)
      (Val i1 v1) -> return (Val i1 $ applyValue v1 $ value valueArgs, env)
      _ -> empty

-- TODO
evalWithEnv env (Let i defs term) = snoc firstSteps (saveLastD evals (substituteEnv newEnv term,newEnv) )
  where
    firstSteps = fmap (\ (termN, envN) -> (Let i (updateDefs envN) termN, envN)) evals
    updateDefs newEnv = zipWith
      (\ (Def info n _) index -> Def info n ( bLookup index newEnv ))
      defs
      (Bound <$> reverse  [0..(length defs - 1) ]) --TODO can in constant space (without reverse)
    newEnv =foldl' (\ envN (Def _ _ tn ) -> bInsert tn envN ) env defs
    evals = evalWithEnv newEnv term

evalWithEnv env t@(Var _ b) =
    let newEnvs = updateEnv b env
        (updatedEnv,newestEnv) = splitLast newEnvs
    in if nullD newEnvs
            then empty
            else case bMaybeLookup b newestEnv of
                Nothing -> empty
                Just v -> snoc (fmap (t,) updatedEnv) (v,newestEnv)
evalWithEnv _ _ = empty

-- variable to update
-- how much deeper the term is then env
updateEnv ::Show i => Bound ->  BruijnEnv (BruijnTerm i) -> DList (BruijnEnv (BruijnTerm i))
updateEnv b env = case bMaybeLookup b env of
    Just term -> case term of
            (Var _ b2)  | b == b2 -> empty
                        | otherwise ->
                            let newEnvs = updateEnv b2 env
                                newestEnv = saveLastD newEnvs env
                            in  snoc newEnvs (bReplace b (bLookup b2 newestEnv) newestEnv)
            _ -> uncurry (bReplace b)<$> evalWithEnv env term
    Nothing -> empty

-- TODO make type substi = BruijnEnv (BruijnTerm i)
substituteEnv :: Show i => BruijnEnv (BruijnTerm i) -> BruijnTerm i -> BruijnTerm i
substituteEnv env term
    | bNull env = term
    | otherwise = go 0 env term
  where go :: Show i => Int ->  BruijnEnv (BruijnTerm i) -> BruijnTerm i -> BruijnTerm i
        go depth e (Lambda i n t) = Lambda i n $ go (depth + 1) e t
        go _     _ t@Val {} = t
        go depth e (Appl left right) = Appl (go depth e left) (go depth e right)

        go depth e t@(Var _ (Bound n))
            | n >= depth = case bMaybeLookup (Bound (n-depth)) e of
                               Nothing -> t
                               Just v -> inc depth v
            | otherwise = t
        go depth e (Let i defs t)  = Let i defs $ go (depth + length defs) e t

type Scope i = BruijnEnv (BruijnTerm i)

dropNew :: Scope i -> Scope i -> Scope i
dropNew  BruijnState {bruijnDepth = olddepth} newscope = newscope {bruijnDepth=olddepth}

splitLast :: DList a -> (DList a , a)
splitLast dList
    | null dList =  error "splitLast cant cplit empty list"
    | otherwise = go empty $ toList dList
  where go _ [] = error "this should never happen, internal error splitLast"
        go begining [a] = (begining ,a)
        go beginning (x:end) = go (snoc beginning x)  end

nullD :: DList a -> Bool
nullD = null. toList

saveLastD :: DList a -> a -> a
saveLastD = saveLast . toList

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

-- increase every variale name in term that not bound in that therm with increase
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

fullEval :: Show i => BruijnTerm i -> BruijnTerm i
fullEval t = saveLast (fst <$> toList ( evalWithEnv bEmtyEnv t )) t
