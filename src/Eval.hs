{-# LANGUAGE TupleSections, LambdaCase #-}
module Eval 
where
import Debug.Trace
import Control.Monad.State.Strict
import Data.DList
import Data.Maybe
import Data.Bifunctor

import Lambda
import BruijnTerm
import Value
import BruijnEnvironment
import Type

data Ref a = Subst a | Keep  a deriving (Show, Eq)
type Scope i = BruijnEnv (Ref (BruijnTerm i))

unwrap :: Ref a -> a
unwrap (Subst a) = a
unwrap (Keep a ) = a

extract :: Bound  ->  Scope i -> BruijnTerm i
extract b env = unwrap $ bLookup b env

tryExtract :: Bound -> Scope i -> Maybe (BruijnTerm i)
tryExtract b env = unwrap <$> bMaybeLookup b env

ref :: BruijnTerm i -> Ref (BruijnTerm i)
ref term | isValue term = Subst term
         | otherwise = Keep term
--
-- TODO write Test  for correct order
-- |eval term in accordance with call by value.
-- If a term can't be further be evaluated it will return 'Nothing'
eval :: BruijnTerm () -> Maybe (BruijnTerm ())
eval = listToMaybe . evalSteps

evalSteps ::BruijnTerm () -> [BruijnTerm ()]
evalSteps = fmap fst . toList . evalWithEnv bEmtyEnv

-- TODO make result  (DList,end ) ore other thrick to nut use last
--TODO fix names
evalWithEnv :: Scope () -> BruijnTerm () -> DList (BruijnTerm (),Scope ())
evalWithEnv env  term | traceShow (term,env)  False = undefined
evalWithEnv env (Appl func args) = (firstFullExpr `append` nextFullExpr ) `append` final valueFunc envArg
  where

    -- applyEnv (t,e) = (substituteEnv e t,e)

    evalFunc = evalWithEnv env func
    firstFullExpr = (\(t,envN) -> (Appl t (substituteEnv envN args),envN)) <$> evalFunc
    (valueFunc,newerEnv) = saveLastD  evalFunc (func,env)

    evalArgs = evalWithEnv newerEnv args
    nextFullExpr = first (Appl (substituteEnv newerEnv valueFunc )) <$> evalArgs
    (valueArgs,envArg) = saveLastD evalArgs (args,newerEnv)

    final func env = traceShow (func,valueArgs) $ case func of
      (Lambda _ _ t1) -> let newestEnv = bInsert (Subst $ substituteEnv env valueArgs) env
                             fixEnv (Bound b) term = if isValue $ unwrap term --FIXME
                                                     then term
                                                     else Subst $ Var () $ Bound (b - 1)
                         in cons (substituteEnv (mapWithBound fixEnv newestEnv) t1,env ) $
                            second bDropLevel  <$> evalWithEnv newestEnv t1
      (Val i1 v1) -> return (Val i1 $ applyValue v1 $ value $ substituteEnv env valueArgs, env)
      (Var _ b)  -> let (outscope,inscope) = bSplitAt b env
                    in second (bAppend outscope) <$> final (extract b env) inscope
      _ -> empty

evalWithEnv env (Let info defs term) = second bDropLevel <$> firstSteps `append` final  (saveLastD evals (substituteEnv dumyEnv term,newEnv))
  where
    firstSteps = fmap (uncurry prependLet) evals
    prependLet _term _env = (Let info (updateDefs _env) _term, _env)
    updateDefs _env = zipWith
      (\ (Def _info n _) index -> Def _info n $ extract index _env)
      defs
      (defsBounds defs)
    newEnv =  bInserts (reverse $ fmap (Keep . substituteEnv dumyEnv . implementation) defs ) env
    dumyEnv :: Scope  ()
    dumyEnv = bInserts ( Keep . Var () <$> reverse (defsBounds  defs )) env --TODO find clean solution
    evals = evalWithEnv newEnv term
    final result = case result of
        (v@Val {},_env) -> singleton (v, _env)
        (Lambda _info n t,_env) ->
                let newt = swap (length defs) $ substituteEnv env1 t
                    env1 = bInsert  (Keep $ Var undefined (Bound 0)) _env
                in singleton $ first (Lambda _info n) (uncurry prependLet ( newt, _env))
        (Var _ b,_env)-> case tryExtract b _env of
               Nothing -> empty
               Just Var {}->error "not in scope" 
               Just v -> final (v,_env)
        _ ->  empty

evalWithEnv env t@(Var _ b) =
    let newEnvs = updateEnv b env
    in if nullD newEnvs
            then empty
            else let newestEnv = last $ toList  newEnvs
                 in  ((t,) <$> fromList (init $ toList newEnvs ))`append` (
                    case tryExtract b newestEnv of
                        Nothing -> error "cant happen"
                        Just v -> singleton(v,newestEnv))
evalWithEnv _ _ = empty

-- variable to update
updateEnv ::Bound -> Scope () -> DList (Scope ())
updateEnv (Bound b)  env | traceShow  (b, env) False = undefined
updateEnv b env = case tryExtract b env of
    Just term -> case term of
            (Var _ b2)  ->
            -- case bMaybeLookup b2 env of
                -- Nothing -> empty
                -- Just term2 ->
                    let (outScoop, inScope)  = bSplitAt b env
                        newInScope = updateEnv b2 inScope
                        newEnvs = bAppend outScoop  <$> newInScope
                        newestInScope = saveLastD newInScope inScope
                        newestEnv = saveLastD newEnvs env
                    in  snoc newEnvs (bReplace b (bLookup b2 newestInScope ) newestEnv)
            _ -> let (outScoop, inScope)  = bSplitAt b env
                     result =  evalWithEnv inScope term
                 in uncurry (bReplace b) . bimap Subst (bAppend outScoop)<$> result
    Nothing -> empty


--TODO remove depth , replace in env?
-- wont substitute if term in env is not value
substituteEnv :: Scope () -> BruijnTerm () -> BruijnTerm ()
substituteEnv env term
    | not $ any
            (\case
                Subst {} -> True
                _ -> False)
            (bList env) = term
    | otherwise = go 0 env term
  where newEnv = env -- mapWithBound  (\ b a -> if isValue a then a else Var () b) env go :: Int -> BruijnEnv (BruijnTerm ()) -> BruijnTerm () -> BruijnTerm ()
        go depth e (Lambda i n t) = Lambda i n $ go (depth + 1) e t
        go _     _ t@Val {} = t
        go depth e (Appl left right) = Appl (go depth e left) (go depth e right)

        go depth e t@(Var _ (Bound n))
            | n >= depth = case bMaybeLookup (Bound (n-depth)) e of
                               Just (Subst v) -> incFree depth v
                               _ -> t
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

isValue :: BruijnTerm i -> Bool
isValue Var {} = False --TODO check
isValue Val {} = True
isValue Lambda {} = True
isValue Appl {} = False
isValue Let {} = False

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
