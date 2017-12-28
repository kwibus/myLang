{-#LANGUAGE FlexibleContexts #-}
module TypeCheck where

import Control.Exception.Base (assert)
import qualified Data.IntMap as IM
import qualified Data.Set as Set
import Control.Monad.State.Strict hiding (sequence)
import Data.Bifunctor
import Data.Maybe

import ErrorCollector
import Value
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
 where finedNewName :: Free -> FreeEnv Free -> Int -> (Free, (FreeEnv Free, Int))
       finedNewName f env n = case fMaybeLookup f env of
             Just fname  -> (fname, (env, n))
             Nothing -> (Free n, (finsertAt (Free n ) f env, n + 1))
       go :: Type -> FreeEnv Free -> Int -> (Type,(FreeEnv Free,Int))
       go (TVar f ) env n = first TVar (finedNewName f env n)
       go (TPoly f ) env n = first TPoly (finedNewName f env n)
       go (TAppl t1 t2 ) env n = let (t1', ( env', n' )) = go t1 env n
                                     (t2', ( env'', n'')) = go t2 env' n'
                                 in (TAppl t1' t2', ( env'', n''))
       go (TVal t1) e n = (TVal t1, (e, n))

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

solver :: BruijnTerm i -> ErrorCollector [TypeError i] Type
solver e =
  let (result,subs) = runInfer $ solveWith e
  in fmap ( close . (apply subs) ) result

type Infer i a = ErrorCollectorT [TypeError i] ( State InferState ) a -- TODO check is this option order of transformers
data InferState = InferState {fresh :: Int, substitution :: TSubst, context ::TEnv }
type TSubst = FreeEnv Type
type TEnv = BruijnEnv (Either [Free] Type)

runInfer :: Infer i a -> (ErrorCollector [TypeError i] a,TSubst)
runInfer infer = second substitution $ runState (runErrorT infer) $ InferState 0 fEmtyEnv bEmtyEnv

newFreeVar :: Infer i Free
newFreeVar = do
    i <- gets fresh
    modify' $ \s->s{fresh = i + 1}
    return $ Free i

-- TODO which types whould be poly and test
-- TODO add comments (name algoritme symtrye asumptions (apply))
-- TODO maybe need a rewrite
--      *  use unionfind
--      *  consider order checks
--              now individual defs are checked separate
--              and after ward the subs are merged, and it is checked if the new defs are correcly used in onther defs
--              this is inconsistend with check of final term let
--              where the correct type of defs is input
--              which one gives best error messages or is fastest
solveWith :: BruijnTerm i -> Infer i Type
solveWith e@(Let _ defs e2) = do -- TODO vorbid type some type of self refrence
  modify' $ \s -> s{context = bInserts (replicate (length defs) $ Left []) (context s)}
  size <- bSize <$> gets context
  typeDefs <- mapM (\(Def _ _ t) -> solveWith t<* reset size) defs  -- zip (defsBounds defs) defs
  tenv <- gets context
  let polys = map (generalize tenv) typeDefs
  zipWithM_ (\b t ->
      case bLookup b tenv of
        Right _ -> error "already devined"
        Left useses -> forM useses $ \use->
          do
              freshT <- instantiate t
              unifyM e freshT $ TVar use
      ) (defsBounds polys ) polys
  zipWithM_ (\b t -> do
      newt <- applyM t
      modify' $ \s ->  s{context = bReplace b (Right newt) (context s)}
      ) (defsBounds polys ) polys
  solveWith e2
  where
    reset size = modify' $ \s -> s {context = bDrop (bSize (context s) - size )(context s)}

solveWith (Lambda _ _ e2) = do
    k <- newFreeVar
    modify' $ \s -> s {context = bInsert (Right $ TVar k) (context s)} --TRY
    t  <- solveWith e2
    applyM (TAppl (TVar k) t)

solveWith e@Appl {} = do
    let (function : args) = accumulateArgs e
    functionTyp <- solveWith function
    argsTyps <- mapM (\ arg -> solveWith arg) args
    var <- newFreeVar
    let typeArg = foldr1 TAppl (argsTyps ++ [TVar var])
    unifyM e functionTyp typeArg
    applyM (TVar var)

solveWith (Val _ v) = return $ getType v

solveWith (Var i n) = do
    tenv <- gets context
    case bMaybeLookup n tenv of
      Just (Right pt ) -> do
          t <- instantiate pt
          applyM t
      (Just (Left used)) -> do
          v <- newFreeVar
          modify' $ \s ->s{context = bReplace n (Left (v:used))tenv}
          return (TVar v)
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
    toTVar conversion (TPoly (Free i)) = case IM.lookup i conversion of
             Just j -> return (conversion, TVar j)
             Nothing -> newFreeVar >>= ( \ j -> return (IM.insert i j conversion, TVar j))

    toTVar conversion (TAppl t1 t2) = do
         (conversion', t1') <- toTVar conversion t1
         (newconversion, t2') <- toTVar conversion' t2
         return (newconversion, TAppl t1' t2')

    toTVar conversion t = return (conversion, t)

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
    freeInEnv = Set.unions $ map (either Set.fromList typeFreeVars . snd) $ bToList env
    toPoly (TAppl t1 t2 ) = TAppl (toPoly t1) (toPoly t2)
    toPoly (TVar i) | not (Set.member i freeInEnv) = TPoly i
    toPoly t = t

unifyM :: BruijnTerm i -> Type -> Type -> Infer i ()
unifyM origin t1 t2 = do
  t1' <- applyM t1 --TODO writght fast one
  t2' <- applyM t2
  newSubs <- toExcept $ mapError (\ erro -> [UnifyAp origin t1' t2' erro]) $  unify t1 t2
  oldSubs <- gets substitution
  sub <- toExcept $ mapError (\ erros -> [UnifyDef t1' t2' erros]) $ unifySubs newSubs  oldSubs
  modify' $ \s-> s{substitution = sub  }

-- TODO can be replace when we use infer monad smart
-- TODO add comments how it works
-- insert second into the first
unifySubs :: TSubst -> TSubst -> ErrorCollector [UnificationError] TSubst
unifySubs sub1 sub2 = IM.foldrWithKey f (return sub1) sub2
    where f key typ1 (Result sub) = case IM.lookup key sub of
            Nothing -> IM.union sub <$> unify (apply sub typ1 ) (apply sub (TVar (Free key)))
            Just typ2 -> IM.union sub <$> unify (apply sub typ1) (apply sub typ2)
          f key typ1 (Error err ) = case IM.lookup key sub1 of
            Nothing -> throw err
            Just typ2 -> throw err *> unify (apply sub1 typ1) (apply sub1 typ2)

--TODO only changes?
unify :: Type -> Type -> ErrorCollector [UnificationError] TSubst
unify (TPoly _) _ = return fEmtyEnv
unify  _ (TPoly _) = return fEmtyEnv
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

-- TODO make apply that optimize subst map
applyM :: Type ->Infer i Type
applyM t = do
  subs <- gets substitution
  return $ apply subs t

apply :: TSubst -> Type -> Type
apply sub (TPoly i) = fromMaybe (TPoly i) (applyVar sub i)
apply sub (TVar i) = fromMaybe (TVar i) (applyVar sub i)
apply _ (TVal v) = TVal v
apply sub (TAppl t1 t2) =
  let t1' = apply sub t1
      t2' = apply sub t2
  in TAppl t1' t2'

applyVar :: TSubst -> Free -> Maybe Type
applyVar sub f = if fMember f sub
    then assert (not (isIn f (fLookup f sub) )) $
         Just $ apply sub $ fLookup f sub
    else Nothing

bind :: Free -> Type -> Either UnificationError TSubst
bind n1 t
    | TVar n1 == t = return fEmtyEnv
    | infinit n1 t = Left $ Infinit n1 t
    | otherwise = return $ finsertAt t n1 fEmtyEnv

infinit :: Free -> Type -> Bool
infinit var (TAppl t1 t2) = isIn var t1 || isIn var t2
infinit _ _ = False

isIn :: Free -> Type -> Bool
isIn _ TVal {} = False
isIn _ TPoly {} = False
isIn var (TAppl t1 t2) = isIn var t1 || isIn var t2
isIn var1 (TVar var2 ) = var1 == var2

unifys :: Type -> Type -> Bool
unifys t1 t2 = hasSucces $ unify t1 t2
