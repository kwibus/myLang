{-#LANGUAGE FlexibleContexts #-}
module TypeCheck where

import Control.Exception.Base (assert)
import qualified Data.IntMap as IM
import Control.Monad.State hiding ()
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

-- TODO update comments
-- TODO make every variable poly, and maybe enforce in type
close :: TypeA a -> Type
close t = fst $ go t fEmtyEnv 0
 where finedNewName :: Free -> FreeEnv Free -> Int -> (Free, (FreeEnv Free, Int))
       finedNewName f env n = case fMaybeLookup f env of
             Just fname  -> (fname, (env, n))
             Nothing -> (Free n, (finsertAt (Free n ) f env, n + 1))
       go :: TypeA a -> FreeEnv Free -> Int -> (Type,(FreeEnv Free,Int))
       go (TVar f _) env n = first (\f_ -> TVar f_ ()) (finedNewName f env n)
       go (TPoly f _) env n = first (\f_ -> TPoly f_ ())(finedNewName f env n)
       go (TAppl t1 t2 ) env n = let (t1', ( env', n' )) = go t1 env n
                                     (t2', ( env'', n'')) = go t2 env' n'
                                 in (TAppl t1' t2', ( env'', n''))
       go (TVal t1) e n = (TVal t1, (e, n))

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

solver :: BruijnTerm i -> ErrorCollector [TypeError i] Type
solver e =
  let (result,subs) = runInfer $ solveWith e
  in fmap (close . apply subs) result

-- TODO better name
type TypeL = TypeA Int -- ^ type anotated with bruijnen level of the variabe it refers to
type Infer i a = ErrorCollectorT [TypeError i] ( State InferState) a -- TODO check is this option order of transformers
data InferState = InferState { fresh :: Int
                             , substitution :: TSubst TypeL
                             , context :: TEnv -- TODO betername
                             }

type TSubst a = FreeEnv a

-- | stores the type of the variables in scope
--
--  * 'Right' 'TypeL' if type is known
--  * 'Left' level 'place holder' if type is not known.
--          The place holder is stored so correct use can be checked when type become know
type TEnv = BruijnEnv (Either (Int, [Free]) TypeL)

toType :: TypeA a -> Type
toType = mapVar (const ())

runInfer :: Infer i a -> (ErrorCollector [TypeError i] a,TSubst TypeL)
runInfer infer = second substitution $ runState (runErrorT infer) $ InferState 0 fEmtyEnv bEmtyEnv

-- TODO get rid of free in name
newFreeVar :: Infer i Free
newFreeVar = do
    i <- gets fresh
    modify' $ \s->s{fresh = i + 1}
    return $ Free i

newVar  ::  Infer i TypeL
newVar = do
  f <- newFreeVar
  l <- level
  return $ TVar f l

-- TODO level is depend on size env, make code dependent on when you insert britel (instantiate level,)
level :: Infer i Int
level = bruijnDepth <$> gets context

-- TODO which types whould be poly and test
-- TODO add comments (name algoritme symtrye asumptions (apply))
-- TODO maybe need a rewrite
--      *  use unionfind
--      *  consider order checks
solveWith :: BruijnTerm i -> Infer i TypeL
solveWith e@(Let _ defs e2) = do
  currentLevel <- level
  modify' $ \s -> s{context = bInserts (replicate (length defs) $ Left (currentLevel, [])) (context s)}
  zipWithM_ (\ b (Def _ _ t) -> do
      typeDef <- preserve $ solveWith t
      poly <- generalize currentLevel <$> applyM typeDef
      tenv <- gets context
      void $ case bLookup b tenv of
          Right _ -> error "already devined"
          Left (originLevel,uses) -> assert (originLevel == currentLevel) $forM uses $ \f-> do
                newt <- instantiate currentLevel poly
                unifyM e newt $ TVar f currentLevel
      modify' $ \s ->  s{context = bReplace b (Right poly) (context s)}
    ) (defsBounds defs) defs
  solveWith e2

solveWith (Lambda _ _ e2) = do
    k <- newVar
    modify' $ \s -> s {context = bInsert (Right k) (context s)}
    t  <- solveWith e2
    -- applyM (TAppl k t)
    return (TAppl k t)

solveWith e@Appl {} = do
    let (function : args) = accumulateArgs e
    functionTyp <- preserve $ solveWith function
    argsTyps <- mapM (preserve . solveWith) args
    f <- newFreeVar
    let var = TVar f $ typeResultLevel functionTyp
    let typeArg = foldr1 TAppl (argsTyps ++ [var])
    unifyM e functionTyp typeArg
    -- applyM var
    return var

solveWith (Val _ v) = return (mapVar (const 0) $ getType v)

solveWith (Var i n) = do
    tenv <- gets context
    currentLevelevel <- level
    case bMaybeLookup n tenv of
      Just (Right pt ) -> do
          t <- instantiate currentLevelevel pt
          applyM t
      (Just (Left (orginLevel,uses))) -> do
          f  <- newFreeVar
          modify' $ \s ->s{context = bReplace n (Left (orginLevel ,f:uses))tenv}
          return (TVar f orginLevel)
      Nothing -> throwT [ICE $ UndefinedVar n i]

typeResultLevel :: TypeL -> Int
typeResultLevel (TVar _ l) = l
typeResultLevel (TPoly _ l) = l
typeResultLevel TVal {} = 0
typeResultLevel (TAppl _ t) = typeResultLevel t

-- foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
-- foldM1 _ [] = error " foldM1 empty List"
-- foldM1 _ [a] = return a
-- foldM1 f (x : xs) = foldM f x xs

-- | instantiate copys all variabls and replace them with new vars
--
-- >>> runInfer $ instantiate (TVar (Free (-1)) 0 ~> (TPoly (Free (-1)) 0 ))
-- (Result (TAppl (TVar (Free (-1)) 0) (TVar (Free 0) 0)),fromList [])
--
-- need to be applied to work corretly
instantiate :: Int -> TypeL -> Infer a TypeL
instantiate originLevel = fmap snd . toTVar fEmtyEnv
  where
    toTVar :: FreeEnv Free -> TypeL -> Infer a (FreeEnv Free, TypeL)
    toTVar conversion (TPoly (Free i)_) = case IM.lookup i conversion of -- TODO does this use the right level?
             Just j -> return (conversion, TVar j originLevel )
             Nothing -> newFreeVar >>= ( \  f -> return (IM.insert i f conversion, TVar f originLevel ))

    toTVar conversion (TAppl t1 t2) = do
         (conversion', t1') <- toTVar conversion t1
         (newconversion, t2') <- toTVar conversion' t2
         return (newconversion, TAppl t1' t2')

    toTVar conversion t = return (conversion, t)

-- TODO better name
preserve :: Infer i a -> Infer i a
preserve m = do
  originalSize <- bSize <$> gets context
  a <- m
  modify' $ \s -> s {context = bDrop (bSize (context s) - originalSize )(context s) }
  return a

-- | generalize takes a type and converts it to its most polymorfic form/ principle form
-- it should not quantife over variable that are already quantified in the env
-- so:
--
-- >>> generalize 1 (TVar (Free 1) 2 ~> TVar (Free 2) 0 ~> TPoly (Free 3) 0 ~> TVar (Free 4) 2)
-- TAppl (TPoly (Free 1) 2) (TAppl (TVar (Free 2) 0) (TAppl (TPoly (Free 3) 0) (TPoly (Free 4) 2)))
--
-- "Forall a c d . a -> b -> c -> d"
--
-- need to be applied with its substitutions to work correctly

-- TODO which level is that? above scope
generalize :: Int -> TypeL -> TypeL
generalize currentLevel = toPoly
  where
    -- boundinEnv = Set.unions $ map (typeFreeVars. toType) . snd) $ bToList env
    -- toPoly (TVar i levelorigin) | not $ Set.member i boundinEnv = TPoly i levelorigin
    toPoly (TAppl t1 t2 ) = TAppl (toPoly t1) (toPoly t2)
    toPoly (TVar f levelorigin) | levelorigin > currentLevel = TPoly f levelorigin
    toPoly t = t

unifyM :: BruijnTerm i -> TypeL -> TypeL -> Infer i ()
unifyM origin t1 t2 = do
  t1' <- applyM t1 --TODO writght fast one
  t2' <- applyM t2
  oldSubs <- gets substitution
  sub <- toExcept $ mapError (\ erro -> [UnifyAp origin (toType t1') (toType t2') erro]) $ do
        newSubs <- unify t1 t2
        unifySubs oldSubs newSubs
  modify' $ \s-> s{substitution = sub  }

-- TODO can be replace when we use infer monad smart
-- TODO add comments how it works
-- insert second into the first
unifySubs :: Ord a => TSubst (TypeA a) -> TSubst (TypeA a)-> ErrorCollector [UnificationError] (TSubst (TypeA a))
unifySubs sub1 sub2 = IM.foldrWithKey f (return sub1) sub2
    where f key typ1 (Result sub) = case IM.lookup key sub of
            Nothing -> IM.union sub <$> fromEither (bind (Free key) (apply sub typ1))
            Just typ2 -> IM.union sub <$> unify (apply sub typ1) (apply sub typ2)
          f key typ1 (Error err ) = case IM.lookup key sub1 of
            Nothing -> throw err
            Just typ2 -> throw err *> unify (apply sub1 typ1) (apply sub1 typ2)

--TODO only changes?
unify :: Ord a => TypeA a -> TypeA a  -> ErrorCollector [UnificationError] (TSubst (TypeA a))
-- unify (TVar n1 level1) (TVar n2 level2) = --TODO test if this is needed
--   if level1 < level2
--   then fromEither $ bind n2 (TVar n1 level1)
--   else fromEither $ bind n1 (TVar n2 level2)
unify (TVar n _) t = fromEither $ bind n t
unify t (TVar n _) = fromEither $ bind n t
unify TPoly{} _ = return fEmtyEnv
unify _ TPoly{} = return fEmtyEnv
unify (TAppl t11 t12 ) (TAppl t21 t22) =
  do sub1 <- unify t11 t21
     sub2 <- unify t12 t22
     unifySubs sub1 sub2 --TODO
unify t1@(TVal v1) t2@(TVal v2) = if v1 == v2
    then return fEmtyEnv
    else throw [Unify (toType t1) (toType t2) ]
unify t1 t2 = throw [Unify (toType t1) (toType t2)]

-- TODO make apply that optimize subst map
applyM :: TypeL -> Infer i TypeL
applyM t = do
  subs <- gets substitution
  return $ apply subs t

apply :: TSubst (TypeA a) -> TypeA a -> TypeA a
apply _ (TPoly i j) = TPoly i j
apply sub (TVar i j) = fromMaybe (TVar i j) (applyVar sub i)
apply _ (TVal v) = TVal v
apply sub (TAppl t1 t2) =
  let t1' = apply sub t1
      t2' = apply sub t2
  in TAppl t1' t2'

applyVar :: TSubst (TypeA a) -> Free -> Maybe (TypeA a)
applyVar sub f = if fMember f sub
    then assert (not $ isIn f $ fLookup f sub ) $
         Just $ apply sub $ fLookup f sub
    else Nothing

bind :: Free -> TypeA a -> Either UnificationError (TSubst (TypeA a))
bind n1 (TVar n2 _) | n1 == n2 = return fEmtyEnv
bind n1 t
    | infinit n1 t = Left $ Infinit n1 $ toType t
    | otherwise = return $ finsertAt t n1 fEmtyEnv

infinit :: Free -> TypeA a -> Bool
infinit var (TAppl t1 t2) = isIn var t1 || isIn var t2
infinit _ _ = False

isIn :: Free -> TypeA a -> Bool
isIn _ TVal {} = False
isIn _ TPoly {} = False
isIn var (TAppl t1 t2) = isIn var t1 || isIn var t2
isIn var1 (TVar var2 _) = var1 == var2

unifys :: Ord a => TypeA a -> TypeA a -> Bool
unifys t1 t2 = hasSucces $ unify t1 t2
