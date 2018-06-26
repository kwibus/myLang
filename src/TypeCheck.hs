{-# LANGUAGE MonoLocalBinds#-}
module TypeCheck where

import Control.Exception.Base (assert)
import qualified Data.IntMap as IM
import qualified Data.Set as Set
import Control.Monad.State hiding ()
import Control.Monad.Except
import Data.Bifunctor
import Data.Maybe
import Data.Either
import Unsafe.Coerce

import Info
import Value
import Type
import MakeType
import BruijnEnvironment
import BruijnTerm
import LambdaF2
import FreeEnvironment
import TypeError
-- $setup
-- >>> import MakeType

-- -- TODO make every variable poly, and maybe enforce in type?
solver :: BruijnTerm i j -> Either [TypeError i j] Type
solver e =
  let (result,subs) = runInfer $ typecheck e
  in fmap (normalise . toType. apply subs) result --TODO do we always want to normalise

annotate :: BruijnTerm i j -> Either [TypeError i j] (BruijnTerm Type j)
annotate e = let (ast,subs) = runInfer $ annotateType e
             in fmap (mapI (toType .apply subs.dePoly ).fst ) ast -- TODO

-- TODO remove boilerplate
dePoly :: TypeA i -> TypeA i
dePoly (TAppl t1 t2) = TAppl (dePoly t1) (dePoly t2)
dePoly (TPoly i j) = TVar i j
dePoly (TVar i j) = TVar i j
dePoly (TVal a) = TVal a

-- TODO better name
type TypeL = TypeA Int -- ^ type anotated with bruijnen level of the variabe it refers to
type Infer i j a = ExceptT [TypeError i j] ( State InferState) a -- TODO check is this option order of transformers
data InferState = InferState { fresh :: Int
                             , substitution :: TSubst TypeL
                             , context :: TEnv -- TODO betername
                             }

-- | stores the type of the variables in scope
--
--  * 'Right' 'TypeL' if type is known
--  * 'Left' level 'place holder' if type is not known.
--          The place holder is stored so correct use can be checked when type become know
type TEnv = BruijnEnv (Either (Int, [Free]) TypeL)

type TSubst a = FreeEnv a

toType :: TypeA a -> Type
toType = mapVar (const ())

runInfer :: Infer i j a -> (Either [TypeError i j] a,TSubst TypeL)
runInfer infer = second substitution $ runState (runExceptT infer) $ InferState 0 fEmtyEnv bEmtyEnv

newFreeVar :: Infer i j Free
newFreeVar = do
    i <- gets fresh
    modify' $ \s->s{fresh = i + 1}
    return $ Free i

newVar  ::  Infer i j TypeL
newVar = do
  f <- newFreeVar
  l <- level
  return $ TVar f l

-- TODO level is depend on size env, make code dependent on when you insert britel (instantiate level,)
--      define level more prescie
--      agument why whe need it this way
level :: Infer i j Int
level = bruijnDepth <$> gets context

solveDef :: Int -> BruijnTerm i j -> Bound -> Def i TypeL -> Infer i j TypeL
solveDef currentLevel orignal b (Def _ _ t) = do
      -- currentLevel <- level

      tenv <- gets context
      -- subs <- gets substitution
      -- poly <- classicGeneralize b subs <$> applyEnv tenv <*> applyM t
      poly <- generalize currentLevel <$> applyM t
      void $ case bLookup b tenv of
          Right _ -> error "already devined"
          Left (originLevel,uses) ->assert (originLevel == currentLevel ) forM uses $ \f-> do
                newt <- instantiate currentLevel poly
                unifyM orignal newt $ TVar f currentLevel
      modify' $ \s ->  s{context = bReplace b (Right poly) (context s)}
      return poly

solve :: BruijnTerm i j -> LamTermF i j Bound TypeL -> Infer i j TypeL
solve _ (LetF _ _ result) = return result

solve _ (LambdaF ns t) = do --TODO
    (_,vs) <- bSplitAt (length ns ) <$> gets context
    return $ foldr TAppl t $ rights vs

solve orignal (ApplF functionType argsTypes) = do
    f <- newFreeVar
    let var = TVar f $ typeResultLevel functionType
    let typeArg = foldr1 TAppl (argsTypes ++ [var])
    unifyM orignal functionType typeArg
    return var

solve _ (ValF _ v) = return (mapVar (const 0) $ getType v)

solve _ (VarF i n) = do
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
      Nothing -> throwError [ICE $ UndefinedVar n i]

--- TODO which types whould be poly and test
--- TODO add comments (name algoritme symtrye asumptions (apply))
---      *  use unionfind
---      *  consider other order checks
walk :: BruijnTerm i j
     -> (BruijnTerm i j -> LamTermF i j Bound a -> Infer i j a)
     -> (Int -> BruijnTerm i j -> Bound -> Def i a ->Infer i j a) --TODO remove level and maybe original
     -> Infer i j a
walk ast0 f fdef  =  go ast0
  where
    -- go :: BruijnTerm i -> Infer i a
    go ast@Lambda {} = do
      let (ns,_ ) = accumulateVars ast
      let nvars = length ns
      vs <- replicateM nvars newVar
      modify' $ \s -> s {context = bInserts (map Right vs) (context s)}

      astfm <- sequence (go <$> wrap ast)
      a <- f ast astfm
      dropVars nvars

      return a

    go ast@(Let i defs term) = do
      let nvars = length defs
      currentLevel <- level
      modify' $ \s -> s{context = bInserts (replicate (length defs) $ Left (currentLevel, [])) (context s)}
      defs2 <- zipWithM (\b (Def idef n body) -> do
          astf <- go body
          Def idef n <$> fdef currentLevel ast b ( Def idef n astf)
        ) (defsBounds defs) defs
      t <- go term

      a <- f ast (LetF i defs2 t)
      dropVars nvars
      return a
    go ast = do
      astfm <- sequence (go <$> wrap ast)
      f ast astfm

typecheck :: BruijnTerm i j -> Infer i j TypeL
typecheck term = walk term solve solveDef

annotateType :: BruijnTerm i j -> Infer i j (BruijnTerm TypeL j, TypeL )
annotateType term = walk term annotate anotateDef
  where
    annotate :: BruijnTerm i j -> LamTermF i j Bound  (BruijnTerm TypeL j, TypeL) -> Infer i j (BruijnTerm TypeL j, TypeL)
    annotate original astF = do
      typ <- solve original $ snd  <$> astF
      let typedAstF = case astF of
            (LambdaF vars (body,_)) -> LambdaF (zipWith (\t (_,n) -> (t,n)) (accumulateTypes typ) vars ) body
            (LetF i defs result ) -> LetF i (map (\(Def _ n (body, t)) -> Def t n body) defs ) (fst result)
            t -> unsafeCoerce (fmap fst t)
      return (unwrap typedAstF, typ)
    anotateDef :: Int -> BruijnTerm i j -> Bound -> Def i  (BruijnTerm TypeL j, TypeL) -> Infer i j (BruijnTerm TypeL j, TypeL)
    anotateDef currentLevel original b def@(Def _ _ (body, _)) = do
      t <- solveDef currentLevel original b (snd  <$> def)
      return (body ,t)

-- TODO does not work with circulair devinions (you can make let varible with type a which can match a fucntion)
-- TODO does not work well Appl with let Polymorfism, have to use unify to get specialisised van poly to work.
--      But these supstitution are not applied to previos varible
readType :: BruijnTerm Type j -> Type
readType ast0 = go bEmtyEnv ast0
  where
    go :: BruijnEnv Type -> BruijnTerm Type j -> Type
    go _    (Val _ v)  = getType v
    go tEnv (Var _ b) = bLookup b tEnv
    go tEnv (Lambda t _ ast) = t ~> go (bInsert t tEnv )ast
    go tEnv (Appl e1 e2) = case go tEnv e1 of
        (TAppl t1 t2) -> case unify t1 (go tEnv e2) of
            Right subs -> apply subs t2
            Left e -> error $ show e
        _ -> error $ "not a fuction: " ++ show (removeInfo e1)
    go tEnv (Let _ defs result ) = go (foldl (\_tEnv (Def t _ _ ) -> bInsert t _tEnv)tEnv  defs) result

typeResultLevel :: TypeL -> Int
typeResultLevel (TVar _ l) = l
typeResultLevel (TPoly _ l) = l
typeResultLevel TVal {} = 0
typeResultLevel (TAppl _ t) = typeResultLevel t

-- | instantiate copys all variabls and replace them with new vars
--
-- >>> runInfer $ instantiate 0 (TVar (Free (-1)) 0 ~> (TPoly (Free (-1)) 0 ))
-- (Right (TAppl (TVar (Free (-1)) 0) (TVar (Free 0) 0)),fromList [])
--
-- need to be applied to work corretly
instantiate :: Int -> TypeL -> Infer i j TypeL
instantiate originLevel = fmap snd . toTVar fEmtyEnv
  where
    toTVar :: FreeEnv Free -> TypeL -> Infer i j (FreeEnv Free, TypeL)
    toTVar conversion (TPoly (Free i) _) = case IM.lookup i conversion of
             Just j -> return (conversion, TVar j originLevel)
             Nothing -> newFreeVar >>= ( \ f -> return (IM.insert i f conversion, TVar f originLevel))

    toTVar conversion (TAppl t1 t2) = do
         (conversion', t1') <- toTVar conversion t1
         (newconversion, t2') <- toTVar conversion' t2
         return (newconversion, TAppl t1' t2')

    toTVar conversion t = return (conversion, t)

dropVars :: Int -> Infer i j ()
dropVars n = modify' $ \s -> s {context = bDrop n (context s) }

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
-- generalize :: TEnv -> TypeL -> TypeL
generalize :: Int -> TypeL -> TypeL
generalize currentLevel = toPoly
  where

    toPoly (TAppl t1 t2 ) = TAppl (toPoly t1) (toPoly t2)
    toPoly (TVar f levelorigin) | levelorigin > currentLevel = TPoly f levelorigin
    toPoly t = t

classicGeneralize :: Bound -> TSubst TypeL ->  TEnv -> TypeL -> TypeL
classicGeneralize b subs env t = toPoly t
  where
    boundinEnv :: Set.Set Free
    boundinEnv = Set.unions $ map (either
      (Set.unions . map (typeFreeVars .toType. apply subs .flip TVar 0 ). snd )
      (typeFreeVars . toType)) $
      map snd $ bToList $ removeCurrent env

    removeCurrent :: TEnv ->  TEnv
    removeCurrent = bReplace b (Left (0 ,[]))

    toPoly (TVar i levelorigin) | not $ Set.member i boundinEnv = TPoly i levelorigin
    toPoly (TAppl t1 t2 ) = TAppl (toPoly t1) (toPoly t2)
    toPoly t = t

applyEnv :: TEnv -> Infer i j  TEnv
applyEnv tenv = mapM (mapM applyM) tenv

unifyM :: BruijnTerm i j -> TypeL -> TypeL -> Infer i j ()
unifyM origin t1 t2 = do
  t1' <- applyM t1 --TODO writght fast one
  t2' <- applyM t2
  oldSubs <- gets substitution
  sub <- withExceptT (\ erro -> [UnifyAp origin (toType t1') (toType t2') erro]) $ liftEither $ do
        newSubs <- unify t1 t2
        unifySubs oldSubs newSubs
  modify' $ \s-> s{substitution = sub  }

-- TODO can be replace when we use infer monad smart
-- TODO add comments how it works
-- insert second into the first
unifySubs :: Ord a => TSubst (TypeA a) -> TSubst (TypeA a)-> Either[UnificationError] (TSubst (TypeA a))
unifySubs sub1 sub2 = IM.foldrWithKey f (Right sub1) sub2
    where f key typ1 (Right sub) = case IM.lookup key sub of
            Nothing ->  IM.union sub <$> first (:[]) ( bind (Free key) (apply sub typ1))
            Just typ2 -> IM.union sub <$> unify (apply sub typ1) (apply sub typ2)
          f key typ1 (Left err ) = case IM.lookup key sub1 of
            Nothing -> Left err
            Just typ2 -> first (err++) $ unify (apply sub1 typ1) (apply sub1 typ2)

--TODO only changes?
--     use unionfind
unify :: Ord a => TypeA a -> TypeA a  -> Either [UnificationError] (TSubst (TypeA a))
-- unify (TVar n1 level1) (TVar n2 level2) = --TODO test if this is needed
--   if level1 < level2
--   then LiftEither $ bind n2 (TVar n1 level1)
--   else LiftEither $ bind n1 (TVar n2 level2)
unify (TVar n _) t = first (:[]) $ bind n t
unify t (TVar n _) = first(:[]) $ bind n t
unify TPoly{} _ = return fEmtyEnv
unify _ TPoly{} = return fEmtyEnv
unify (TAppl t11 t12 ) (TAppl t21 t22) =
  do sub1 <- unify t11 t21
     sub2 <- unify t12 t22
     unifySubs sub1 sub2 --TODO
unify t1@(TVal v1) t2@(TVal v2) = if v1 == v2
    then return fEmtyEnv
    else Left [Unify (toType t1) (toType t2) ]
unify t1 t2 = throwError [Unify (toType t1) (toType t2)]

-- TODO make apply that optimize subst map
applyM :: TypeL -> Infer i j TypeL
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

-- TODO rename occurs check?
infinit :: Free -> TypeA a -> Bool
infinit var (TAppl t1 t2) = isIn var t1 || isIn var t2
infinit _ _ = False

isIn :: Free -> TypeA a -> Bool
isIn _ TVal {} = False
isIn _ TPoly {} = False
isIn var (TAppl t1 t2) = isIn var t1 || isIn var t2
isIn var1 (TVar var2 _) = var1 == var2

unifys :: Ord a => TypeA a -> TypeA a -> Bool
unifys t1 t2 = isRight $ unify t1 t2
