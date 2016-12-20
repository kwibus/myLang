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
    applyEnv (t,e) = (substituteEnv e t,e)
    firstFullExpr = (\(t,envN) -> (Appl t (substituteEnv envN args),envN)) <$> evalFunc
    (valueFunc,newerEnv) = saveLastD  evalFunc $ applyEnv (func,env)

    evalArgs = evalWithEnv newerEnv args
    nextFullExpr = first (Appl valueFunc) <$> evalArgs
    (valueArgs,envArg) = saveLastD evalArgs $ applyEnv (args,newerEnv)

    final = case valueFunc of
      (Lambda _ _ t1) -> let newestEnv = bInsert valueArgs envArg
                             fixEnv (Bound b) term = if normalForm term
                                                     then term
                                                     else Var () $ Bound (b - 1)
                         in cons (substituteEnv (mapWithBound fixEnv newestEnv) t1,envArg ) $
                            second (bDrop 1) <$> evalWithEnv (fmap (incFree 1) newestEnv) t1
      (Val i1 v1) ->  return (Val i1 $ applyValue v1 $ value $ substituteEnv envArg valueArgs, envArg)
      _ -> empty

evalWithEnv env (Let info defs term) = second (bDrop (length defs)) <$> firstSteps `append` final  (saveLastD evals (substituteEnv dumyEnv term,newEnv))
  where
    firstSteps = fmap (uncurry prependLet) evals
    prependLet _term _env = (Let info (updateDefs _env) _term, _env)
    updateDefs _env = zipWith
      (\ (Def _info n _) index -> Def _info n $ bLookup index _env)
      defs
      (defsBounds defs)
    newEnv = foldl' (\ envN (Def _ _ tn ) -> bInsert (substituteEnv dumyEnv tn) envN ) env defs
    dumyEnv = bExtend (length defs) env
    evals = evalWithEnv newEnv term
    final result = case result of
        (v@Val {},_env) -> singleton (v, _env)
        (Lambda _info n t,_env) -> singleton $ first (Lambda _info n) (uncurry prependLet (swap (length defs) t, _env))
        (Var _ b,_env)-> case bMaybeLookup b _env of
               Nothing -> empty
               Just Var {}  -> empty
               Just v -> final (v,_env)
        _ ->  empty

evalWithEnv env t@(Var _ b) =
    let newEnvs = updateEnv b env
    in if nullD newEnvs
            then empty
            else let newestEnv = last $ toList  newEnvs
                 in  ((t,) <$> fromList (init $ toList newEnvs ))`append` (
                    case bMaybeLookup b newestEnv of
                        Nothing -> empty
                        Just v -> singleton(v,newestEnv))
evalWithEnv _ _ = empty

-- variable to update
updateEnv ::Bound -> Scope () -> DList (Scope ())
updateEnv b env = case bMaybeLookup b env of
    Just term -> case term of
            (Var _ b2)  -> case bMaybeLookup b2 env of
                Nothing -> empty
                Just v -> case v of
                    -- (Var _ _) -> let newEnv = bReplace b v env -- TODO check
                                 --     nextEnv = updateEnv   b newEnv
                                 --     -- lastEnv = saveLastD nextEnv newEnv
                                 -- in -- snoc
                                 --       (cons newEnv  nextEnv)
                                 --    -- (bReplace b (bLookup b2 lastEnv)lastEnv)
                    _ ->let newEnvs = updateEnv b2 env
                            newestEnv = saveLastD newEnvs env
                        in  snoc newEnvs (bReplace b (bLookup b2 newestEnv) newestEnv)
            _ -> uncurry (bReplace b) <$> evalWithEnv env term
    Nothing -> empty


--TODO remove depth , replace in env?
-- wont substitute if term in env is not value
substituteEnv :: Scope () -> BruijnTerm () -> BruijnTerm ()
substituteEnv env term
    | bNull newEnv = term
    | otherwise = go 0 newEnv term
  where newEnv  = bFilter normalForm env
        go :: Int -> BruijnEnv (BruijnTerm ()) -> BruijnTerm () -> BruijnTerm ()
        go depth e (Lambda i n t) = Lambda i n $ go (depth + 1) e t
        go _     _ t@Val {} = t
        go depth e (Appl left right) = Appl (go depth e left) (go depth e right)

        go depth e t@(Var _ (Bound n))
            | n >= depth = case bMaybeLookup (Bound (n-depth)) e of
                               Nothing -> t
                               Just v -> incFree depth v
            | otherwise = t
        go depth e (Let i defs t) = Let i (fmap goDefs defs) $ go' t
            where go' = go (depth + length defs) e
                  goDefs ( Def i' n' t') = Def i' n' $ go' t'

swap :: Int -> BruijnTerm () -> BruijnTerm ()
swap n = go 0
  where go depth (Lambda _ name t) = Lambda () name $ go (depth + 1) t
        go depth (Appl t1 t2) = Appl (go depth t1) (go depth t2)
        go  _    t@Val {} = t
        go depth (Var _ (Bound n2))
            | n2 == depth = Var () (Bound $! depth + n)
            | depth < n2 && depth + n >= n2 = Var () (Bound (n2 -1))
            | otherwise  = Var ()  (Bound n2)
        go depth (Let _ defs term) = Let () (fmap goDefs defs) $ go' term
            where go' = go (depth  + length defs)
                  goDefs (Def i name t) = Def i name $ go' t

-- substitute :: BruijnTerm () ->  BruijnTerm () -> BruijnTerm ()
-- substitute t1 = substituteEnv (bInsert t1 bEmtyEnv)

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

normalForm :: BruijnTerm i -> Bool
normalForm Var {} = True -- False --TODO check
normalForm Val {} = True
normalForm Lambda {} = True
normalForm Appl {} = False
normalForm Let {} = False

-- increase every variale name in term that not bound in that therm with increase
incFree :: Int -> BruijnTerm i -> BruijnTerm i
incFree 0 term = term
incFree increase  term = go 0 term
  where go depth (Lambda i n t) = Lambda i n $ go (depth+1) t
        go depth (Appl t1 t2) = Appl (go depth t1)(go depth t2)
        go depth (Var i (Bound n)) | n >= depth  = Var i $ Bound $ n+increase
                                   | otherwise = Var i (Bound n)
        go depth (Let i defs t) = Let i (fmap incDefs defs) $ go newDepth t
          where
            newDepth = depth +length defs
            incDefs (Def is ns ts) = Def is ns $ go newDepth ts
        go _ (Val i v) = Val i v

fullEval :: BruijnTerm () -> BruijnTerm ()
fullEval t = saveLast (fst <$> toList ( evalWithEnv bEmtyEnv t )) t
