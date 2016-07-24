{-#LANGUAGE FlexibleContexts #-}
module TypeCheck where

import Control.Exception.Base (assert)
import qualified Data.IntMap as IM
import qualified Data.Set as Set
import Control.Monad.State hiding (sequence)
import Data.Bifunctor

import ErrorCollector
import Value
import Lambda
import Type
import BruijnTerm
import BruijnEnvironment
import FreeEnvironment
import TypeError

-- $setup
-- >>> import MakeType

-- TODO make every variable poly, and maybe enforce in type
close :: Type -> Type
close t = fst $ go t fEmtyEnv 0
 where finedNewName:: Free -> FreeEnv Free -> Int ->  (Free , (FreeEnv Free , Int))
       finedNewName f env n = if fMember f env
             then (fLookup f env,( env, n))
             else (Free n,( finsertAt (Free n ) f env, n + 1))
       go (TVar f ) env n = first TVar (finedNewName f env n)
       go (TPoly f ) env n = first TPoly (finedNewName f env n)
       go (TAppl t1 t2 ) env n = let (t1',( env', n' )) = go t1 env n
                                     (t2',( env'', n'')) = go t2 env' n'
                                 in (TAppl t1' t2',( env'', n''))
       go (TVal t1) e n = (TVal t1, (e, n))

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

solver :: BruijnTerm i -> ErrorCollector [TypeError i] Type
solver e = fmap ( close . uncurry (flip apply) ) $ runInfer $ solveWith e fEmtyEnv bEmtyEnv

type Infer i a = ErrorCollectorT [TypeError i] ( State Int ) a
type TSubst = FreeEnv Type
type TEnv = BruijnEnv Type

runInfer :: Infer i a -> ErrorCollector [TypeError i] a
runInfer infer = evalState ( runErrorT infer) 0

newFreeVar :: Infer i Free
newFreeVar = do
    i <- get
    put (i + 1)
    return $ Free i

solveWith :: BruijnTerm i -> TSubst -> TEnv -> Infer i (Type, TSubst)
solveWith e@(Let _ defs e2) sub tenv = do -- TODO vorbid type some type of self refrence
  newVars <- replicateM (length defs) newFreeVar
  let tempTEnv= foldl ( flip ( bInsert. TVar)) tenv newVars
  (polys, subs) <- unzip <$> mapM (solveDefs tempTEnv) defs
  newSubs <- toExcept $ mapError (\erros -> [UnifySubs e erros]) $foldM1 unifySubs  subs
  let newTEnv = foldl ( flip  bInsert) tenv polys
  solveWith e2 newSubs newTEnv
  where solveDefs dic ( Def _ _ en) = do
            (t2, newsub) <- solveWith en sub dic
            let poly = generalize dic t2
            return (poly,newsub)

solveWith (Lambda _ _ e2) sub tenv = do
    k <- newFreeVar
    let newTEnv = bInsert (TVar k) tenv
    (t, newSub) <- solveWith e2 sub newTEnv
    return (apply newSub (TAppl (TVar k) t), newSub)

solveWith e@Appl{} sub tenv = do
    let (function : args) = accumulateArgs e
    (functionTyp, sub1) <- solveWith function sub tenv
    (argsTyps, subs) <- unzip <$> (mapM (\ arg -> solveWith arg sub tenv) args)
    var <- newFreeVar
    let typeArg = foldr1 TAppl (argsTyps ++ [TVar var])
    newsup <- toExcept $ mapError (\erro -> [UnifyAp e functionTyp typeArg erro]) $ unify functionTyp typeArg
    newSub <- toExcept $ mapError (\erros -> [UnifySubs e erros]) $ foldM1 unifySubs (newsup:sub:sub1:subs)
    let newTyp = apply newSub (TVar var)
    return (newTyp,newSub)

solveWith (Val _ v) _ _ = return (getType v, fEmtyEnv)

solveWith (Var i n) sub tEnv = case  bMaybeLookup n tEnv of
        Just pt -> do
            t <- instantiate pt
            return (apply sub t, fEmtyEnv)
        Nothing -> throwT [ICE $ UndefinedVar n i]

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 _ [] = error " foldM1 empty List"
foldM1 _ [a] = return a
foldM1 f (x : xs) = foldM f x xs


-- | instantiate copys all variabls and replace them with new vars
-- >>> runInfer $ instantiate ((tVar (-1)) ~> (TPoly $ Free (-1)))
-- Result (TAppl (TVar (Free (-1))) (TVar (Free 0)))

instantiate :: Type -> Infer a Type
instantiate = fmap snd . toTVar fEmtyEnv
  where
    toTVar :: FreeEnv Free -> Type -> Infer a (FreeEnv Free, Type)
    toTVar conversion  (TPoly (Free i)) = case IM.lookup i conversion  of
             Just j -> return (conversion, TVar j)
             Nothing -> newFreeVar >>= ( \j -> return (IM.insert i j conversion, TVar $ j))

    toTVar conversion (TAppl t1 t2) = do
         (conversion', t1') <- toTVar conversion t1
         (newconversion, t2')<- toTVar conversion' t2
         return (newconversion,TAppl t1' t2')

    toTVar conversion t = return (conversion,t)

-- | generalize takes a type and converts it to its most polymorfic form/ principle form
-- it should not quantife over variable that are already quantified in the env
-- so:
--
-- >>> generalize (bInsert (tVar 2 ~> (TPoly (Free 3)))bEmtyEnv) (tVar 1 ~> tVar 2 ~> TPoly (Free 3) ~> tVar 4)
-- TAppl (TPoly (Free 1)) (TAppl (TVar (Free 2)) (TAppl (TPoly (Free 3)) (TPoly (Free 4))))
--
-- "Forall a c d . a -> b -> c -> d"

generalize :: TEnv -> Type -> Type
generalize env = toPoly
  where
    freeInEnv = Set.unions $ map freeVars $ bToList env
    toPoly (TAppl t1 t2 ) = TAppl (toPoly t1) (toPoly t2)
    toPoly (TVar i) | not (Set.member i freeInEnv) = TPoly i
    toPoly t = t

freeVars :: Type -> Set.Set Free
freeVars (TVar v ) = Set.singleton v
freeVars (TAppl t1 t2) = freeVars t1 `Set.union` freeVars t2
freeVars _ = Set.empty

-- instanceOf :: PolyType -> PolyType -> _
-- instanceOf (Forall v1 t1 ) (Forall v2 t2 ) = undefined

unifySubs :: TSubst -> TSubst -> ErrorCollector [UnificationError] TSubst
unifySubs sub1 sub2 = IM.foldWithKey f (return sub1) sub2
    where f key typ1 (Result sub) = case IM.lookup key sub of
            Nothing ->  fmap (IM.union sub) $ unify (apply sub typ1 ) (apply sub (TVar (Free key)))
            Just typ2 -> fmap (IM.union sub) $ unify (apply  sub typ1) (apply sub typ2)
          f key typ1 (Error err ) = case IM.lookup key sub1 of
            Nothing -> throw err
            Just typ2 -> throw err *> unify (apply sub1 typ1) (apply sub1 typ2)

--TODO only changes?
unify :: Type -> Type -> ErrorCollector [UnificationError] TSubst
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
isIn _ TPoly {} = False
isIn var (TAppl t1 t2) = isIn var t1 || isIn var t2
isIn var1 (TVar var2 ) = var1 == var2

unifys :: Type -> Type -> Bool
unifys t1 t2 = hasSucces $ unify t1 t2
