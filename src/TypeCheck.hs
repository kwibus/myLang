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
import TypeError

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

solver :: BruijnTerm i -> Either (TypeError i )(Type Bound)
solver e = fmap ( \ (_,t ,env)-> close  (apply t env)) $ solveWith e fEmtyEnv bEmtyEnv

solveWith :: BruijnTerm i -> FreeEnv (i,Type Free ) -> BruiEnv (i ,Free) ->
    Either (TypeError i) (i,Type Free, FreeEnv (i,Type Free))
solveWith (Lambda i _ e2) env dic = do
    let (env1, k) = newFreeVar env
    let (dic1, _) = bInsert (i,k) dic
    (i2,t2, env2) <- solveWith e2 env1 dic1
    return $ (i,apply (TAppl (TVar k) t2) env2, env2)

solveWith (Appl i e1 e2) env dic = do
    (i1,t1, env1) <- solveWith e1 env dic -- preverence left
    (i2,t2, env2) <- solveWith e2 env1 dic
    let (env3, var) = newFreeVar env2
    env4 <- unify (i1,apply t1 env3) (i2,apply (TAppl t2 (TVar var)) env3) env3
    return (i,apply (TVar var) env4 , env4)

solveWith (Val i v) env _ = return (i,ftype v, env)
solveWith (Var i n) env dic = if bMember n dic
        then return (i,apply ( TVar (snd (bLookup n dic))) env, env)
        else throwError $ ICE (undefined ) -- i n )

unify :: (i,Type Free) -> (i,Type Free) -> FreeEnv (i,Type Free) ->
    Either (TypeError i) (FreeEnv (i,Type Free)) -- retunr type
unify (i1,TVar n) (i2,t) env = bind (i1,n) (i2,t) env
unify (i1,t) (i2,TVar n) env = bind (i2,n) (i1,t) env
-- unify (Lambda _ t1 ) t2 env = unify t1 t2 env  -- for full F
-- unify t2 (Lambda _ t1 ) env = unify t1 t2 env
unify (i1,TAppl t11 t12 ) (i2,TAppl t21 t22) env =
  do env2 <- unify (i1, t11) (i2,t21) env
     env3 <- unify (i1,(apply t12 env2)) (i2,(apply t22 env2)) env2
     return (env3)
unify (i1,TVal v1) (i2,TVal v2) env = if (v1 == v2)
    then return env
    else throwError VarVar 
unify t1 t2 env = throwError $  Unify t1 t2 env

-- TODO remove either
apply :: Type Free -> FreeEnv (i,Type Free) -> (Type Free)
apply (TVar i) env = if fMember i env
    then apply (snd (fLookup i env)) env
    else TVar i
apply (TAppl t1 t2) env =
  let t1' = apply t1 env
      t2' = apply t2 env
  in TAppl t1' t2'
apply t _ = t

bind :: (i,Free) -> (i,Type Free) -> FreeEnv (i,Type Free) ->
     Either (TypeError i )(FreeEnv (i,Type Free))
bind (i1, n1) (i2,t) env
    | TVar n1 == t = return env
    | fMember n1 env = unify (fLookup n1 env) (i2,t) env
    | infinit n1 t env = throwError $ Infinit (i1,n1) (i2,t) env
    | otherwise = case t of
        TVar n2 -> if fMember n2 env
                        then unify (i1,TVar n1 ) (i2,snd (fLookup n2 env)) env
                        else return $ finsertAt (i2,t) n1 env
        _ -> return $ finsertAt (i2,t)n1 env

infinit :: Free -> (Type Free) -> FreeEnv (i,Type Free) -> Bool
infinit var (TAppl t1 t2) env = isIn var t1 env || isIn var t2 env
infinit _ TVal {} _ = False
infinit var1 (TVar var2 ) env =
    fMember var2 env && let var2' = snd (fLookup var2 env)
                        in if var2 == var1 then isIn var1 var2' env
                                          else infinit var1 var2' env

isIn :: Free -> (Type Free) -> FreeEnv (i,Type Free) -> Bool
isIn _ TVal {} _ = False
isIn var (TAppl t1 t2) env = isIn var t1 env || isIn var t2 env
isIn var1 (TVar var2 ) env = var1 == var2 || if fMember var2 env
    then isIn var1 (snd(fLookup var2 env)) env
    else False

unifys :: (i,Type Free) -> (i,Type Free) -> FreeEnv (i,Type Free) -> Bool
unifys t1 t2 e = isRight $ unify t1 t2 e
