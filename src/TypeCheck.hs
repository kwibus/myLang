{-#LANGUAGE FlexibleContexts #-}
module TypeCheck where

import Control.Exception.Base (assert)
import qualified Data.IntMap as IM
import Control.Monad.State hiding (sequence)

import ErrorCollector
import Value
import Lambda
import Type
import BruijnTerm
import BruijnEnvironment
import FreeEnvironment
import TypeError

close :: Type -> Type
close t = fst3 $ go t fEmtyEnv 0
 where go (TVar free ) env n = if fMember free env
             then (TVar $ fLookup free env, env, n)
             else (TVar (Free n), finsertAt (Free n ) free env, n + 1)
       go (TAppl t1 t2 ) env n = let (t1', env', n' ) = go t1 env n
                                     (t2', env'', n'') = go t2 env' n'
                                 in (TAppl t1' t2', env'', n'')
       go (TVal t1) e n = (TVal t1, e, n)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

solver :: BruijnTerm i -> ErrorCollector [TypeError i] Type
solver e = fmap ( close . uncurry apply) $ runInfer $ solveWith e fEmtyEnv bEmtyEnv

type Infer i a = ErrorCollectorT [TypeError i] ( State Int ) a

runInfer :: Infer i a -> ErrorCollector [TypeError i] a
runInfer infer = evalState ( runErrorT infer) 0

newFreeVar :: Infer i Free
newFreeVar = do
    i <- get
    put (i + 1)
    return $ Free i

solveWith :: BruijnTerm i -> FreeEnv Type -> BruijnEnv Free -> Infer i (Type, FreeEnv Type)
solveWith e@(Let _ defs e2) env dic = do
  newVars <- replicateM (length defs) newFreeVar
  let newDic = foldl (flip bInsert) dic newVars
  envs <- mapM (solveDefs newDic ) $ zip newVars defs
  newEnvs <- toExcept $ mapError (\ uError -> [UnifyEnv e uError] ) $ foldM1 unifyEnv envs
  solveWith e2 newEnvs newDic
  where -- solveDefs :: BruijnEnv Free -> (Free, Def i Bound ) -> Infer i (FreeEnv Type)
        solveDefs newDic (v1, Def _ _ en) = do
            (t2, env1) <- solveWith en env newDic
            toExcept $ mapError (\ l -> [UnifyAp e (TVar v1) t2 l ]) $ unify (TVar v1) t2

solveWith (Lambda _ _ e2) env dic = do
    k <- newFreeVar
    let dic1 = bInsert k dic
    (t2, env2) <- solveWith e2 env dic1
    return (apply (TAppl (TVar k) t2) env2, env2)

solveWith e@Appl{} env dic = do
    let (function : args) = accumulateArgs e
    (functionTyp, env1) <- solveWith function env dic
    (argsTyps, envs) <- unzip <$> (mapM (\ arg -> solveWith arg env dic) args)

    var <- newFreeVar
    let typeArg = foldr1 TAppl (argsTyps ++ [TVar var])
    newsup <- toExcept $ mapError (\erro -> [UnifyAp e functionTyp typeArg erro]) $ unify functionTyp typeArg
    newEnv <- toExcept $ mapError (\erros -> [UnifyEnv e erros]) $ foldM1 unifyEnv (newsup:env:env1:envs)
    -- newEnv <- toExcept $ mapError (\erro -> [UnifyAp e functionTyp typeArg erro]) $ unifyEnv combinedEnv newsup
    let newTyp = apply (TVar var ) newEnv
    return (newTyp,newEnv)

solveWith (Val _ v) env _ = return (getType v, env)

solveWith (Var i n) env dic = if bMember n dic
        then return (apply ( TVar (bLookup n dic)) env, env)
        else throwT [ICE $ UndefinedVar n i]

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 _ [] = error " foldM1 empty List"
foldM1 _ [a] = return a
foldM1 f (x : xs) = foldM f x xs

unifyEnv :: FreeEnv Type -> FreeEnv Type -> ErrorCollector [UnificationError] (FreeEnv Type)
unifyEnv env1 env2 = IM.foldWithKey f (return env1) env2
    where f key typ1 (Result env) = case IM.lookup key env of
            Nothing ->  fmap (IM.union env) $unify (apply typ1 env) (apply (TVar (Free key)) env)
            Just typ2 -> fmap (IM.union env) $ unify (apply typ1 env ) (apply typ2 env)
          f key typ1 (Error err ) = case IM.lookup key env1 of
            Nothing -> throw err
            Just typ2 -> throw err *> unify (apply typ1 env1 ) (apply typ2 env1 )

--TODO only changes?
unify :: Type -> Type -> ErrorCollector [UnificationError] (FreeEnv Type) -- retunr type
unify (TVar n) t = fromEither $ bind n t
unify t (TVar n) = fromEither $ bind n t
unify (TAppl t11 t12 ) (TAppl t21 t22) =
  do env1 <- unify t11 t21
     env2 <- unify t12 t22
     unifyEnv env1 env2
unify t1@(TVal v1) t2@(TVal v2) = if v1 == v2
    then return fEmtyEnv
    else throw [Unify t1 t2 ]
unify t1 t2 = throw [Unify t1 t2 ]

apply :: Type -> FreeEnv Type -> Type
apply (TVar i) env = if fMember i env
    then assert (not (isIn i (fLookup i env) )) $
         apply (fLookup i env) env
    else TVar i
apply (TAppl t1 t2) env =
  let t1' = apply t1 env
      t2' = apply t2 env
  in TAppl t1' t2'
apply t _ = t

bind ::  Free -> Type ->  Either UnificationError (FreeEnv Type )
bind n1 t
    | TVar n1 == t = return fEmtyEnv
    | infinit n1 t = Left $ Infinit n1 t
    | otherwise = return $ finsertAt t n1 fEmtyEnv

infinit :: Free -> Type ->  Bool
infinit var (TAppl t1 t2) = isIn var t1 || isIn var t2
infinit _ _ = False

isIn :: Free -> Type -> Bool
isIn _ TVal {} = False
isIn var (TAppl t1 t2) = isIn var t1 || isIn var t2
isIn var1 (TVar var2 ) = var1 == var2

unifys :: Type -> Type -> Bool
unifys t1 t2 = hasSucces $ unify t1 t2
