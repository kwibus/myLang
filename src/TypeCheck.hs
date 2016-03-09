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
solver e = fmap ( close . uncurry  (flip apply)) $ runInfer $ solveWith e fEmtyEnv bEmtyEnv

type Infer i a = ErrorCollectorT [TypeError i] ( State Int ) a
type TSubst = FreeEnv Type
type TEnv = BruijnEnv Free

runInfer :: Infer i a -> ErrorCollector [TypeError i] a
runInfer infer = evalState ( runErrorT infer) 0

newFreeVar :: Infer i Free
newFreeVar = do
    i <- get
    put (i + 1)
    return $ Free i

solveWith :: BruijnTerm i -> TSubst -> TEnv -> Infer i (Type, TSubst)
solveWith e@(Let _ defs e2) sub tenv = do
  newVars <- replicateM (length defs) newFreeVar
  let newTEnv = foldl ( flip bInsert) tenv newVars
  subs <- mapM (solveDefs newTEnv ) $ zip newVars defs
  let  newSubs = sub
  -- newSubs <- toExcept $ mapError (\ uError -> [UnifySub e uError] ) $ foldM1 unifySub subs
  solveWith e2 newSubs newTEnv
  where -- solveDefs :: BruijnSub Free -> (Free, Def i Bound ) -> Infer i (FreeEnv Type)
        solveDefs newTEnv (v1, Def _ _ en) = do
            (t2, sub1) <- solveWith en sub newTEnv
            toExcept $ mapError (\ l -> [UnifyAp e (TVar v1) t2 l ]) $ unify (TVar v1) t2

solveWith (Lambda _ _ e2) sub tenv = do
    k <- newFreeVar
    let tenv1 = bInsert k tenv
    (t2, sub2) <- solveWith e2 sub tenv1
    return (apply sub2 (TAppl (TVar k) t2), sub2)

solveWith e@Appl{} sub tenv = do
    let (function : args) = accumulateArgs e
    (functionTyp, sub1) <- solveWith function sub tenv
    (argsTyps, subs) <- unzip <$> (mapM (\ arg -> solveWith arg sub tenv) args)

    var <- newFreeVar
    let typeArg = foldr1 TAppl (argsTyps ++ [TVar var])
    newsup <- toExcept $ mapError (\erro -> [UnifyAp e functionTyp typeArg erro]) $ unify functionTyp typeArg
    newSub <- toExcept $ mapError (\erros -> [UnifySubs e erros]) $ foldM1 unifySubs (newsup:sub:sub1:subs)
    let newTyp = apply newSub (TVar var )
    return (newTyp,newSub)

solveWith (Val _ v) sub _ = return (getType v, sub)

solveWith (Var i n) sub tenv = if bMember n tenv
        then return (apply sub (TVar (bLookup n tenv)), sub)
        else throwT [ICE $ UndefinedVar n i]

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 _ [] = error " foldM1 empty List"
foldM1 _ [a] = return a
foldM1 f (x : xs) = foldM f x xs

unifySubs :: TSubst-> TSubst -> ErrorCollector [UnificationError] TSubst
unifySubs sub1 sub2 = IM.foldWithKey f (return sub1) sub2
    where f key typ1 (Result sub) = case IM.lookup key sub of
            Nothing ->  fmap (IM.union sub) $unify (apply sub typ1 ) (apply sub (TVar (Free key)) )
            Just typ2 -> fmap (IM.union sub) $ unify (apply sub typ1) (apply sub typ2)
          f key typ1 (Error err ) = case IM.lookup key sub1 of
            Nothing -> throw err
            Just typ2 -> throw err *> unify (apply sub1 typ1) (apply sub1 typ2)

--TODO only changes?
unify :: Type -> Type -> ErrorCollector [UnificationError] TSubst -- retunr type
unify (TVar n) t = fromEither $ bind n t
unify t (TVar n) = fromEither $ bind n t
unify (TAppl t11 t12 ) (TAppl t21 t22) =
  do sub1 <- unify t11 t21
     sub2 <- unify t12 t22
     unifySubs sub1 sub2
unify t1@(TVal v1) t2@(TVal v2) = if v1 == v2
    then return fEmtyEnv
    else throw [Unify t1 t2 ]
unify t1 t2 = throw [Unify t1 t2 ]

apply ::  TSubst -> Type -> Type
apply sub (TVar i) = if fMember i sub
    then assert (not (isIn i (fLookup i sub) )) $
         apply sub $ fLookup i sub
    else TVar i
apply sub (TAppl t1 t2) =
  let t1' = apply sub t1
      t2' = apply sub t2
  in TAppl t1' t2'
apply _ t = t

bind ::  Free -> Type ->  Either UnificationError TSubst
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
