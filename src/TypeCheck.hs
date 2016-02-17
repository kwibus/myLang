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
            toExcept $ mapError (\ l -> [UnifyAp e (TVar v1) t2 l ]) $ unify (TVar v1) t2 env1

solveWith (Lambda _ _ e2) env dic = do
    k <- newFreeVar
    let dic1 = bInsert k dic
    (t2, env2) <- solveWith e2 env dic1
    return (apply (TAppl (TVar k) t2) env2, env2)

solveWith e @(Appl _ e1 e2) env dic = do
    -- let (function : args) = accumulateArgs e
    -- (t1, env1) <- solveWith function env dic
    -- (t2, env2) <- map (\ arg -> solveWith args env dic) args
    -- newenv <- toExcept $ throwT (UnifyEnv e) $ unifyEnv env1 env2
    (t1, env1) <- solveWith e1 env dic
    (t2, env2) <- solveWith e2 env dic
    newenv <- toExcept $ mapError (\ erro -> [UnifyEnv e erro]) $ unifyEnv env1 env2
    var <- newFreeVar
    let t11 = apply t1 newenv
    let t12 = apply (TAppl t2 (TVar var)) newenv
    case unify t11 t12 newenv of
        Error err -> throwT [UnifyAp e t11 t2 err]
        Result env4 -> return (apply (TVar var ) env4, env4)

solveWith (Val _ v) env _ = return (getType v, env)

solveWith (Var i n) env dic = if bMember n dic
        then return (apply ( TVar (bLookup n dic)) env, env)
        else throwT [ICE $ UndefinedVar i n]

repeatM :: Monad m => m a -> m [a]
repeatM = sequence . repeat

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 _ [] = error " foldM1 empty List"
foldM1 _ [a] = return a
foldM1 f (x : xs) = foldM f x xs

unifyEnv :: FreeEnv Type -> FreeEnv Type -> ErrorCollector [UnificationError i] (FreeEnv Type)
unifyEnv env1 env2 = IM.foldWithKey f (return env1) env2
    where f key typ1 (Result env) = case IM.lookup key env of
            Nothing -> unify typ1 (TVar (Free key)) env
            Just typ2 -> unify typ1 typ2 env
          f key typ1 (Error err ) = case IM.lookup key env1 of
            Nothing -> throw err
            Just typ2 -> throw err *> unify typ1 typ2 env1

--TODO only changes?
unify :: Type -> Type -> FreeEnv Type -> ErrorCollector [UnificationError i] (FreeEnv Type) -- retunr type
unify (TVar n) t env = bind n t env
unify t (TVar n) env = bind n t env
-- unify (Lambda _ t1 ) t2 env = unify t1 t2 env  -- for full F
-- unify t2 (Lambda _ t1 ) env = unify t1 t2 env
unify (TAppl t11 t12 ) (TAppl t21 t22) env =
  do env2 <- unify t11 t21 env
     unify (apply t12 env2) (apply t22 env2) env2
unify (TVal v1) (TVal v2) env = if v1 == v2
    then return env
    else throw [VarVar]
unify t1 t2 env = throw [Unify (apply t1 env) (apply t2 env) env]

apply :: Type -> FreeEnv Type -> Type
apply (TVar i) env = if fMember i env
    then assert (not (isIn i (fLookup i env) env)) $
         apply (fLookup i env) env
    else TVar i
apply (TAppl t1 t2) env =
  let t1' = apply t1 env
      t2' = apply t2 env
  in TAppl t1' t2'
apply t _ = t

bind ::  Free -> Type -> FreeEnv Type -> ErrorCollector [UnificationError i] (FreeEnv Type )
bind n1 t env
    | TVar n1 == t = return env
    | fMember n1 env = unify (fLookup n1 env) t env
    | infinit n1 t env = throw [ Infinit n1 t env]
    | otherwise = case t of
        TVar n2 -> if fMember n2 env
                        then unify (TVar n1 ) (fLookup n2 env) env -- TODO check can remove unify and make use of either instead of ErrorCollector
                        else return $ finsertAt t n1 env
        _ -> return $ finsertAt t n1 env

infinit :: Free -> Type -> FreeEnv Type -> Bool
infinit var (TAppl t1 t2) env = isIn var t1 env || isIn var t2 env
infinit _ TVal {} _ = False
infinit var1 (TVar var2 ) env =
    fMember var2 env && let var2' = fLookup var2 env
                        in if var2 == var1 then isIn var1 var2' env
                                          else infinit var1 var2' env

isIn :: Free -> Type -> FreeEnv Type -> Bool
isIn _ TVal {} _ = False
isIn var (TAppl t1 t2) env = isIn var t1 env || isIn var t2 env
isIn var1 (TVar var2 ) env = var1 == var2 || (fMember var2 env && isIn var1 (fLookup var2 env) env)

unifys :: Type -> Type -> FreeEnv Type -> Bool
unifys t1 t2 e = hasSucces $ unify t1 t2 e
