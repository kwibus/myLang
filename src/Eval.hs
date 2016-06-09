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

type Scope i = BruijnEnv (BruijnTerm i)
--
-- TODO write Test  for correct order
-- |eval term in accordance with call by value.
-- If a term can't be further be evaluated it will return 'Nothing'
eval :: BruijnTerm () -> Maybe (BruijnTerm ())
eval = listToMaybe . evalSteps

evalSteps ::BruijnTerm () -> [BruijnTerm ()]
evalSteps = fmap fst . toList . evalWithEnv bEmtyEnv

--TODO make result  (DList,end ) ore other thrick to nut use last
--TODO fix names
evalWithEnv :: Scope () -> BruijnTerm () -> DList (BruijnTerm (),Scope ())
evalWithEnv env (Appl func args) = (firstFullExpr `append` nextFullExpr ) `append` final
  where
    evalFunc = evalWithEnv env func
    firstFullExpr = (\(t,envN) -> (Appl t (substituteEnv envN args),env)) <$> evalFunc
    (valueFunc,envfunction) = saveLastD  evalFunc (func,env)
    newerEnv = dropNew env envfunction

    evalArgs = evalWithEnv newerEnv args
    nextFullExpr = first (Appl valueFunc) <$> evalArgs
    (valueArgs,envArg) = saveLastD evalArgs (args,newerEnv)

    final = case valueFunc of
      (Lambda _ _ t1) -> let newestEnv = bInsert valueArgs envArg
                         in cons (substituteEnv newestEnv t1,newestEnv) (evalWithEnv newestEnv  t1)
      (Val i1 v1) ->  return (Val i1 $ applyValue v1 $ value $substituteEnv envArg valueArgs, envArg)
      _ -> empty

-- TODO
evalWithEnv env (Let info defs term) =  firstSteps `append` final
  where
    firstSteps = fmap (uncurry prependLet) evals
    prependLet _term _env = (Let info (updateDefs _env) _term, env)
    updateDefs _env = zipWith
      (\ (Def _info n _) index -> Def _info n ( bLookup index _env ))
      defs
      (Bound <$> reverse  [0..(length defs - 1) ]) --TODO can in constant space (without reverse)
    newEnv =foldl' (\ envN (Def _ _ tn ) -> bInsert tn envN ) env defs
    evals = evalWithEnv newEnv term
    final = case (\(t,envN) -> (substituteEnv envN t,envN)) $saveLastD evals (term,newEnv) of
        result@(Val {},_) -> singleton result
        (Var {},_)-> empty
        (Lambda _info n t,_env) -> singleton $ first (Lambda _info n) (uncurry prependLet (substitute (Var () (Bound (length defs))) 0 t, _env))
        result ->  singleton (uncurry prependLet result)

evalWithEnv env t@(Var _ b) =
    let newEnvs = updateEnv b env
        (updatedEnv,newestEnv) = splitLast newEnvs
    in if nullD newEnvs
            then empty
            else case bMaybeLookup b newestEnv of
                Nothing -> empty
                Just v -> append ((t,) <$> snoc updatedEnv newestEnv) $ evalWithEnv newestEnv v
evalWithEnv _ _ = empty

-- variable to update
updateEnv ::Bound -> Scope () -> DList (Scope ())
updateEnv b env = case bMaybeLookup b env of
    Just term -> case term of
            (Var _ b2)  | b == b2 -> empty -- TODO remove without creating infinit loop
                        | otherwise ->
                            let newEnvs = updateEnv b2 env
                                newestEnv = saveLastD newEnvs env
                            in  snoc newEnvs (bReplace b (bLookup b2 newestEnv) newestEnv)
            _ -> uncurry (bReplace b)<$> evalWithEnv env term
    Nothing -> empty

substituteEnv :: Scope () -> BruijnTerm () -> BruijnTerm ()
substituteEnv env term
    | bNull env = term
    | otherwise = go 0 env term
  where go :: Int -> BruijnEnv (BruijnTerm ()) -> BruijnTerm () -> BruijnTerm ()
        go depth e (Lambda i n t) = Lambda i n $ go (depth + 1) e t
        go _     _ t@Val {} = t
        go depth e (Appl left right) = Appl (go depth e left) (go depth e right)

        go depth e t@(Var _ (Bound n))
            | n >= depth = case bMaybeLookup (Bound (n-depth)) e of
                               Nothing -> t
                               Just v -> inc depth v
            | otherwise = t
        go depth e (Let i defs t)  = Let i defs $ go (depth + length defs) e t

-- TODO remove initial index
substitute :: BruijnTerm i -> Int-> BruijnTerm i -> BruijnTerm i
substitute t1 n1 t2@(Var i (Bound n2)) | n1 == n2 = inc n1 t1
                               | n1 < n2 = Var i $ Bound (n2-1)
                               | otherwise = t2
substitute t1 n1 (Lambda i n2 t2) = Lambda i n2 $
                    substitute t1 (n1 + 1) t2
substitute t n (Appl t1 t2) = Appl (substitute t n t1) (substitute t n t2)
substitute _ _ t2 = t2

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

value :: BruijnTerm () -> Value
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

fullEval :: BruijnTerm () -> BruijnTerm ()
fullEval t = saveLast (fst <$> toList ( evalWithEnv bEmtyEnv t )) t
