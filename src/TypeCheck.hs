{-#LANGUAGE FlexibleContexts #-}
module TypeCheck where

import Control.Exception.Base (assert)
import qualified Data.IntMap as IM
import qualified Data.Set as Set
import Control.Monad.State hiding (sequence)
import Data.Foldable (traverse_)

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
solver e = fmap ( close . uncurry (flip apply)) $ runInfer $ solveWith e fEmtyEnv bEmtyEnv

type Infer i a = ErrorCollectorT [TypeError i] ( State InferState ) a
type TSubst = FreeEnv Type
type TEnv = BruijnEnv PolyType
type ArgList = FreeEnv Alias
data InferState = InferState { newvar::Int
                             , arglist :: ArgList
                             } deriving Show

type Alias = [Free]


runInfer :: Infer i a -> ErrorCollector [TypeError i] a
runInfer infer = evalState ( runErrorT infer) initState
  where
    initState = InferState 0 fEmtyEnv

newFreeVar :: Infer i Free
newFreeVar = do
    s <- get
    let i = newvar s
    put s{newvar = i+1}
    return $ Free i


solveWith :: BruijnTerm i -> TSubst -> TEnv -> Infer i (Type, TSubst)
solveWith e@(Let _ defs e2) sub tenv = do
  newVars <- replicateM (length defs) newFreeVar
  traverse_ addArglist newVars
  let tempTEnv= foldl ( \tenvN v ->  bInsert (Forall [v] $ TVar v) tenvN) tenv newVars
  (polys, subs) <- unzip <$> mapM (solveDefs tempTEnv) defs
  newSubs <- toExcept $ mapError (\erros -> [UnifySubs e erros]) $foldM1 unifySubs  subs
  let newTEnv = foldl ( flip  bInsert) tenv polys
  zipWithM_ (unifyDefinitionUse newSubs) newVars $ map unPoly polys
  solveWith e2 newSubs newTEnv
  where solveDefs dic ( Def _ _ en) = do
            (t2, newsub) <- solveWith en sub dic
            let poly = generalize dic t2
            return (poly,newsub)
        -- unifyDefinitionUse ::TSubst -> Free -> Type -> Infer i ()
        unifyDefinitionUse subs (Free i) t = do
             state <- get
             let alias = arglist state IM.!  i
             toExcept $ mapError (\erros -> [UnifySubs e erros]) $
                        traverse_ (unify t . apply subs.TVar) alias
             put state{arglist=IM.delete i (arglist state) }

solveWith (Lambda _ _ e2) sub tenv = do
    k <- newFreeVar
    let newTEnv = bInsert (Forall [] $TVar k) tenv
    (t, newSub) <- solveWith e2 sub newTEnv
    return (apply newSub (TAppl (TVar k) t), newSub)

solveWith e@Appl{} sub tenv = do
    let (function : args) = accumulateArgs e
    (functionTyp, sub1) <- solveWith function sub tenv
    (argsTyps, subs) <- unzip <$> mapM (\ arg -> solveWith arg sub tenv) args
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

unPoly :: PolyType -> Type
unPoly (Forall _ t) = t

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 _ [] = error " foldM1 empty List"
foldM1 _ [a] = return a
foldM1 f (x : xs) = foldM f x xs

-- | instantiate copys all qualified variabls and replace them with new vars
--  and ad the var to the alias in arglist
-- >>> runInfer $ instantiate $ Forall [Free 1] (tVar 2 ~> tVar 1)
-- Result (TAppl (TVar (Free 2)) (TVar (Free 0)))

instantiate ::  PolyType -> Infer i Type
instantiate (Forall vs t) = do
  vs' <- mapM (const newFreeVar) vs
  let s = fFromList $ zip (map TVar vs') vs
  zipWithM_ addAlias vs vs'
  return $ apply s t

addArglist:: Free ->  Infer i ()
addArglist (Free f) = modify (\s-> s{arglist = IM.insert f [] (arglist s)})

addAlias :: Free -> Free -> Infer a ()
addAlias (Free orignal) alias = modify (\s ->
                    s{arglist= IM.adjust (alias :) orignal (arglist s)})

-- | generalize takes a type and converts it to its most polymorfic form/ principle form
-- it should not quantife over variable that are already quantified in the env
-- so:
--
-- >>> pShowPoly $ generalize (bInsert (Forall [Free 3] (tVar 2 ~> tVar 3))bEmtyEnv) (tVar 1 ~> tVar 2 ~> tVar 3 ~> tVar 4)
--"Forall a c d . a -> b -> c -> d"

generalize :: TEnv -> Type -> PolyType
generalize env t  = Forall vs t
  where vs = Set.toList $ freeVars  t `Set.difference`  freeInEnv
        freeInEnv :: Set.Set Free
        freeInEnv = Set.unions $ map (freeVarsPoly . snd) $ bToList env

freeVarsPoly :: PolyType-> Set.Set Free
freeVarsPoly (Forall bv t) =  freeVars t `Set.difference` Set.fromList bv

freeVars :: Type -> Set.Set Free
freeVars (TVar v ) = Set.singleton v
freeVars (TAppl t1 t2) = freeVars t1 `Set.union` freeVars t2
freeVars _ = Set.empty

unifySubs :: TSubst -> TSubst -> ErrorCollector [UnificationError] TSubst
unifySubs sub1 sub2 = IM.foldWithKey f (return sub1) sub2
    where f key typ1 (Result sub) = case IM.lookup key sub of
            Nothing -> IM.union sub <$> unify (apply sub typ1 ) (apply sub (TVar (Free key)))
            Just typ2 -> IM.union sub <$> unify (apply  sub typ1) (apply sub typ2)
          f key typ1 (Error err ) = case IM.lookup key sub1 of
            Nothing -> throw err
            Just typ2 -> throw err *> unify (apply sub1 typ1) (apply sub1 typ2)

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
