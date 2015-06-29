module TypeCheck where

import qualified Data.IntMap  as IM
import Control.Monad.Error.Class
import Data.Either.Unwrap
import Control.Monad.State hiding (sequence)
import Control.Monad.Except

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
solver e = fmap ( close .(uncurry apply)) $ runInfer $solveWith e fEmtyEnv bEmtyEnv

type Infer i a = ExceptT (TypeError i ) ( State Int ) a

runInfer ::Infer i a ->  Either (TypeError i) a
runInfer infer = evalState ( runExceptT infer) 0

newFreeVar :: Infer i Free
newFreeVar = do
    i <- get
    put (i + 1)
    return $ Free i

solveWith :: BruijnTerm i -> FreeEnv (Type Free ) -> BruiEnv Free ->
    Infer i (Type Free, FreeEnv (Type Free))
solveWith (Lambda _ _ e2) env dic = do
    k <- newFreeVar
    let dic1 = bInsert  k dic
    (t2, env2) <- solveWith e2 env dic1
    return $ (apply (TAppl (TVar k) t2) env2, env2)

solveWith e@(Appl _ e1 e2) env dic = do
    (t1, env1) <- solveWith e1 env dic -- preverence left
    (t2, env2) <- solveWith e2 env dic
    newenv <- toExcept $ mapLeft (UnifyEnv e) $ unifyEnv env1 env2
    var <- newFreeVar
    let t11 = (apply t1 newenv)
    let t12 = (apply (TAppl t2 (TVar var)) newenv)
    case unify t11 t12 newenv of
        Left err -> throwError $ UnifyAp e t11 t2  err
        Right env4 -> return (apply (TVar var ) env4,env4)

solveWith (Val _ v) env _ = return (ftype v, env)
solveWith (Var i n) env dic = if bMember n dic
        then return (apply ( TVar (bLookup n dic)) env, env)
        else throwError $ ICE $ UndefinedVar i n

-- replace with libary funciont
toExcept ::Monad m =>  Either a b -> ExceptT a m b
toExcept eith = case eith of
    Left e -> throwError e
    Right a -> return a

unifyEnv :: FreeEnv (Type Free) -> FreeEnv (Type Free) -> Either [UnificationError i] (FreeEnv (Type Free))
unifyEnv env1 env2 = IM.foldWithKey f (Right env1) env2
    where f key typ1 (Right env) = case  IM.lookup key env of
            Nothing -> return $ IM.insert key typ1 env
            Just typ2 -> mapLeft (:[]) $ unify typ1 typ2 env
          f key typ1 (Left  err ) = case  IM.lookup key env1 of
            Nothing -> Left err
            Just typ2 -> mapLeft (:err )$ unify typ1 typ2 env1

unify :: (Type Free) -> (Type Free) -> FreeEnv (Type Free) ->
    Either (UnificationError i) (FreeEnv (Type Free)) -- retunr type
unify (TVar n) (t) env = bind n t env
unify t (TVar n) env = bind n t env
-- unify (Lambda _ t1 ) t2 env = unify t1 t2 env  -- for full F
-- unify t2 (Lambda _ t1 ) env = unify t1 t2 env
unify (TAppl t11 t12 ) (TAppl t21 t22) env =
  do env2 <- unify t11 t21 env
     env3 <- unify (apply t12 env2) (apply t22 env2) env2
     return (env3)
unify (TVal v1) (TVal v2) env = if (v1 == v2)
    then return env
    else throwError VarVar
unify t1 t2 env = throwError $  Unify (apply t1 env) (apply  t2 env) env

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
     Either (UnificationError i )(FreeEnv (Type Free))
bind  n1 t env
    | TVar n1 == t = return env
    | fMember n1 env = unify (fLookup n1 env) t env
    | infinit n1 t env = throwError $ Infinit n1 t env
    | otherwise = case t of
        TVar n2 -> if fMember n2 env
                        then unify (TVar n1 ) (fLookup n2 env) env
                        else return $ finsertAt t n1 env
        _ -> return $ finsertAt t n1 env

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
