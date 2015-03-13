module TypeCheck
    ( solver
    , solveWith
    , unify
    , unifys
    , close
    , apply
    ) where

import Control.Monad.Error
import Data.Either

import Vallue
import Lambda
import Type
import BruijnTerm
import Enviroment

close :: Type Free -> Type Bound
close t = fst3 $ go t fEmtyEnv 0
 where go (TVar free ) env n = if fMember free env
             then (TVar $ fLookup free env, env, n)
             else (TVar (Bound n), finsertAt (Bound n ) free env, n + 1)
       go (TAppl t1 t2 ) env n = let (t1', env', n' ) = go t1 env n
                                     (t2', env'', n'') = go t2 env' n'
                                 in (TAppl t1' t2', env'', n'')
       go (TVal t1) e n = (TVal t1, e, n)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

solver :: BruijnTerm Vallue -> Either String (Type Bound)
solver e = fmap ( close . uncurry apply ) $ solveWith e fEmtyEnv bEmtyEnv

solveWith :: BruijnTerm Vallue -> FreeEnv (Type Free ) -> BruiEnv Free ->
    Either String (Type Free, FreeEnv (Type Free))
solveWith (Lambda _ e2) env dic = do
    let (env1, k) = newFreeVar env
    let (dic1, _) = bInsert k dic
    (t2, env2) <- solveWith e2 env1 dic1
    return $ (apply (TAppl (TVar k) t2) env2, env2)

solveWith (Appl e1 e2) env dic = do
    (t1, env1) <- solveWith e1 env dic -- preverence left
    (t2, env2) <- solveWith e2 env1 dic
    let (env3, var) = newFreeVar env2
    env4 <- unify (apply t1 env3) (apply (TAppl t2 (TVar var)) env3) env3
    return (apply (TVar var) env4 , env4)

solveWith (Val v) env _ = return (ftype v, env)
solveWith (Var i) env dic = if bMember i dic
        then return (apply ( TVar (bLookup i dic)) env, env)
        else throwError "fuck"

unify :: (Type Free) -> (Type Free) -> FreeEnv (Type Free) ->
    Either String (FreeEnv (Type Free)) -- retunr type
unify (TVar i) t env = bind i t env
unify t (TVar i) env = bind i t env
-- unify (Lambda _ t1 ) t2 env = unify t1 t2 env  -- for full F
-- unify t2 (Lambda _ t1 ) env = unify t1 t2 env
unify (TAppl t11 t12 ) (TAppl t21 t22) env =
  do env2 <- unify t11 t21 env
     env3 <- unify (apply t12 env2) (apply t22 env2) env2
     return (env3)
unify (TVal v1) (TVal v2) env = if (v1 == v2)
    then return env
    else throwError "error"
unify t1 t2 env = throwError $
    show "unify t1 :: " ++ show t1 ++ " t2 ::" ++ show t2 ++ " env\n"
         ++ show env ++ "\n"

-- TODO remove either
apply :: Type Free -> FreeEnv (Type Free) -> Type Free
apply (TVar i) env = if fMember i env
    then apply (fLookup i env) env
    else TVar i
apply (TAppl t1 t2) env =
  let t1' = apply t1 env
      t2' = apply t2 env
  in TAppl t1' t2'
apply t _ = t

bind :: Free -> Type Free -> FreeEnv (Type Free) ->
     Either String (FreeEnv (Type Free))
bind i1 t1 env
    | TVar i1 == t1 = return env
    | fMember i1 env = unify (fLookup i1 env) t1 env
    | infinit i1 t1 env = throwError $ "infintType"
    | otherwise = case t1 of
        TVar i2 -> if fMember i2 env
                        then unify (TVar i1 ) (fLookup i2 env) env
                        else return $ finsertAt t1 i1 env
        _ -> return $ finsertAt t1 i1 env

infinit :: Free -> (Type Free) -> FreeEnv (Type Free) -> Bool
infinit var (TAppl t1 t2) env = isIn var t1 env || isIn var t2 env
infinit _ TVal {} _ = False
infinit var1 (TVar var2 ) env =
    fMember var2 env && let var2' = fLookup var2 env
                        in if var2 == var1 then isIn var1 var2' env
                                          else infinit var1 var2' env

isIn :: Free -> (Type Free) -> FreeEnv (Type Free) -> Bool
isIn _ TVal {} _ = False
isIn var (TAppl t1 t2) env = isIn var t1 env || isIn var t2 env
isIn var1 (TVar var2 ) env = var1 == var2 || if fMember var2 env
    then isIn var1 (fLookup var2 env) env
    else False

unifys :: Type Free -> Type Free -> FreeEnv (Type Free) -> Bool
unifys t1 t2 e = isRight $ unify t1 t2 e
