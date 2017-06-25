{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Eval
  -- ( eval
  -- , evalSteps
  -- , fullEval
  -- , applyValue
  -- , evalWithEnv
  -- , trans
  -- , trans'
  -- )
where
-- import Control.Monad.Writer
import Control.Monad.State.Strict as Strict
import Control.Monad.Writer.Lazy
import Debug.Trace
import Unprocessed
import Data.Maybe
import Data.Bifunctor
-- import BottumUp
import BruijnTerm
import Value
import BruijnEnvironment
import Type
import ModifiedLambda (MTable, empty,)
import qualified TaggedLambda as Tag
-- import TaggedLambda hiding (LamTerm,Def)

import LambdaF
import Lambda as Lam

--TODO fix names

type Step w a = Writer [w] a
-- -- |eval term in accordance with call by value.
-- -- If a term can't be further be evaluated it will return 'Nothing'
eval :: BruijnTerm () -> Maybe (BruijnTerm ())
eval = listToMaybe . evalSteps

fullEval :: BruijnTerm () -> BruijnTerm ()
fullEval = fst . evalStepsW

evalSteps :: BruijnTerm () -> [BruijnTerm ()]
evalSteps ast = snd $ evalStepsW  ast

evalStepsW :: BruijnTerm () -> (BruijnTerm (),[BruijnTerm()])
evalStepsW term = runWriter $ evalW bEmtyEnv $ Un empty $ Tag.tag term

censorMap :: MonadWriter [w] m => (w -> w) -> m w -> m w
censorMap f = censor (map f) . fmap f

write ::  MonadWriter [w] m => w -> a -> m a
write w a = tell [w] >> return a

produce ::  MonadWriter [a] m => a -> m a
produce a = tell [a] >> return a

retell :: (w1 -> w2) ->  Step w1 a -> Step w2 a
retell f = mapWriterT (fmap $ second (map f))

censors ::  MonadWriter [w] m => (w->w) -> m a -> m a
censors f = censor (map f)

incrementalM :: (b -> a -> Step a (b, a)) -> b -> [a] -> Step [a] b
incrementalM _ b [] = return b -- error "incrementalM does work on empty list"
incrementalM f b [a] = fst <$> retell (: []) ( f b a)
incrementalM f b (a : as) = do
  (newB,newA) <- retell (:as) $ f b a
  censors (newA :) $ incrementalM f newB as

type Env = BruijnEnv (Int ,BruijnTerm ())

-- TODO add coment
maybeExtract ::  Bound -> Env -> Maybe Unprocessed
maybeExtract b@(Bound n) env = do
    (ofset,ast) <- bMaybeLookup b env
    return (reproces $ incFree (n-ofset) ast)

extract :: Bound -> Env -> Unprocessed
extract b env = fromMaybe  (error "variable not in Env") $ maybeExtract b env

store:: [BruijnTerm ()] -> Env -> Env
store  terms env = bInserts (zip (fromToZero (length terms - 1)) terms ) env

update :: Bound -> BruijnTerm () -> Env -> Env
update b t env = bReplace b (ofset,t) env
  where (ofset,_) =  bLookup b env

--TODO use test from evalStep
-- fullEval' :: MonadReader MTable m => Env -> Old -> m (BruijnTerm ())
-- fullEval' env ast = trans ast go
--   where
--     -- go :: LamTermF () Bound Old -> m (BruijnTerm ())
--     go (LambdaF _ n t) = Lambda () n <$> procesM t
--     go (VarF _ b) = return $ bLookup b env
--     go (ValF _ v) = return $ Val () v
--     go (LetF _ defs t) = do
--         (newEnv,newDefs) <- fullEvalDefs' env defs
--         newT <-  fullEval' newEnv t
--         case newT of  -- TODO lazyniss shoue make this fast
--             (Val () v) -> return $ Val () v
--             _ -> return $ Let () newDefs newT
--     go (ApplF t1 t2) = do
--       newT2 <- fullEval' env t2
--       trans t1 (\t1' -> case t1' of
--         (LetF _ defs t) -> do
--             (newEnv,newDefs) <- fullEvalDefs' env defs
--             newT <- reduceAndEval newEnv t newT2
--             case newT of  -- TODO lazyniss shoue make this fast
--                 (Val () v) -> return $ Val () v
--                 _ -> return $ Let () newDefs newT
--         _ ->do
--           newT1 <-fullEval' env t1
--           reduceAndReEval env newT1 newT2
--         )
--     reduceAndReEval env newT1 newT2= reproces (\t1 -> reduceAndEval env t1 newT2) (Tag.tag newT1)
--     reduceAndEval env t1 t2= fullEval' env =<< betaReduction t1 t2
--
-- betaReduction :: MonadReader MTable m => Old -> BruijnTerm () -> m Old
-- betaReduction t1 newT2 = peekM t1 $ \case
--   (LambdaF _ _ t) -> return $substitute newT2 t
--   (ValF _ v1) -> case newT2  of  -- TODO lazyniss shoue make this fast but could make specialised version
--       (Val () v2) -> return $ Old $ Tag.tag $ Val () $ applyValue v1 v2
--       _ -> error $ show newT2 ++ " is not a value"
--   _ -> error $ "eval is not sound:" ++ show t1
--
-- fullEvalDefs' :: MonadReader MTable m => Env -> [DefF () Bound Old] -> m (Env,[Def () Bound])
-- fullEvalDefs' env defs = do
--     (_,newEnv,newDefs) <-
--       foldM (\(b,envN,defsN) (DefF i n t)-> do
--           vn <- fullEval' envN t
--           return (b-1,bReplace (Bound b) vn envN, Def i n vn:defsN ))
--       (nDefs-1,bInsertBlackhole 1 env,[]) -- TODO to many blackholes
--       defs
--     return (newEnv,newDefs)
--   where
--     nDefs = length defs

evalW :: Env -> Unprocessed -> Step (BruijnTerm()) (BruijnTerm())
evalW env ast = case peek ast of
    (LambdaF _ n t) -> return $ Lambda () n $ proces t
    (VarF _ b) -> case maybeExtract b env of
        Just v -> produce $ proces v
        Nothing -> return $ Var () b
    (ValF _ v) -> return $ Val () v
    (LetF _ defs t) -> do
      let t' = proces t
      (newEnv,newDefs) <- retell (\defs -> Let () defs t') $ evalDefsW env defs
      newT <-  censors (Let () newDefs) $ evalW newEnv t
      case newT of  -- TODO lazyniss shoue make this fast
           (Val () v) -> produce $ Val () v
           _ -> return $ Let () newDefs newT
    (ApplF t1 t2) -> do
      let oldT1 = proces t1
      newT2 <- censors (Appl oldT1) $ evalW env t2
      reduce env t1 newT2

  where
    -- shallowEval :: Env -> Unprocessed -> Step (BruijnTerm ()) Unprocessed
    -- shallowEval env t = -- traceShow "shallow" $
    --   case peek t of
    --     (ApplF t1 t2) -> do
    --         let oldT1 = proces t1 -- TODO lazyniss shoue make this fast
    --         newT2 <- censors (Appl oldT1) $ evalW env t2
    --         shallowReduc env t1 newT2
    --     (VarF _ b ) ->
    --       let newTerm = bLookup b env
    --       in  tell[newTerm] >>
    --           return (reproces newTerm)
    --     _ -> return t

    shallowReduc :: Env -> Unprocessed -> BruijnTerm () -> Step (BruijnTerm ()) Unprocessed
    shallowReduc env t1 newT2 = case peek t1  of
    --     (LambdaF _ _ t) ->
    --       let newterm =substitute newT2 t
    --           t' = proces newterm
    --       in tell [t']>> shallowEval env newterm
    --
    --     (ApplF t11 t12) -> do
    --         let oldT11 = proces t11 -- TODO lazyniss shoue make this fast
    --         newT12 <- censors (\t12' -> Appl (Appl oldT11 t12') newT2) $ evalW env t12
    --         t1' <- censors ( `Appl` newT2) $  shallowReduc env t11 newT12
    --         shallowReduc env t1' newT2
        _ -> reproces <$> reduce env t1 newT2

    reduce :: Env -> Unprocessed -> BruijnTerm () -> Step (BruijnTerm ()) (BruijnTerm ())
    reduce env t1 newT2 =
      case peek t1 of
        (LambdaF _ _ t) ->
          let newterm =substitute newT2 t
              t' = proces newterm
          in tell [t']>> evalW env newterm

        (ValF _ v1) -> case newT2  of  -- TODO lazyniss shoue make this fast but could make specialised version
          (Val () v2) -> do
            let newTerm =  Val () $ applyValue v1 v2
            tell [newTerm]
            return $ newTerm
          _ -> error $ "applied " ++ show v1 ++ " with " ++ show newT2
        (ApplF t11 t12) -> do
            let oldT11 = proces t11 -- TODO lazyniss shoue make this fast
            newT12 <- censors (\t12' -> Appl (Appl oldT11 t12') newT2) $ evalW env t12
            t1' <- censors ( `Appl` newT2) $ shallowReduc env t11 newT12
            reduce env t1' newT2
        (VarF _ b ) ->
          let newTerm = extract b env
          in censors (`Appl`newT2) ( tell[proces newTerm]) >>
              reduce env newTerm newT2

        (LetF _ defs t12) -> do
          let t12' = proces t12
          (newEnv,newDefs) <- retell (\defs ->Appl ( Let () defs t12') newT2) $ evalDefsW env defs
          newT12 <- censors (\newT12' -> Appl (Let () newDefs newT12') newT2 )$ (reproces <$> evalW newEnv t12)

          case peek newT12 of  -- TODO lazyniss shoue make this fast
            (ValF () v) ->
              tell [Appl (Val () v) newT2] >>
              reduce env newT12 newT2
            _ -> do
              newT1 <-censors (Let () newDefs )$ reduce newEnv newT12 newT2
              case newT1 of
                  (Val () v) -> produce (Val () v)
                  _ -> return $ Let () newDefs newT1
        -- _ -> reduceAndEval env t1 newT2

    -- reduceAndEval :: Env -> Unprocessed -> BruijnTerm () -> Step (BruijnTerm ()) (BruijnTerm ())
    -- reduceAndEval env t1 t2 = do
    --     newT <- reduce env t1 t2
    --     evalW env newT

-- procesEither :: MonadReader MTable m => (Old  -> m a) -> Either Old (BruijnTerm ()) -> m a
-- procesEither f = either f (reproces f)

--TODO replace
saveLast :: a  -> [a] -> a
saveLast a as = last (a:as)

evalDefsW :: Env -> [DefF ()  Bound Unprocessed] ->  Step [Def () Bound ] (Env,[Def () Bound ] )
evalDefsW env defs = do
    let procesedDef = map (procesDef) defs
    let dumyEnv = store (map Lam.implementation procesedDef) env
    ((_,newEnv),defsS) <-listen $ incrementalM go (nDefs-1,dumyEnv) procesedDef
    return (newEnv,saveLast procesedDef defsS)
  where
    nDefs = length defs
    go :: (Int,Env) -> Def () Bound -> Step (Def () Bound) ((Int,Env),Def () Bound)
    go (n,envN) (Def () b t) = do
      v <- retell (Def () b) $ evalW envN $ reproces t
      return ((n-1,update (Bound n) v envN),Def () b v)

traceShowIdM :: (Show w, Show a) => Step w a -> Step w a
traceShowIdM m = do
  (a,w) <- listen m
  traceShow (a,w)$ return a

value :: Lam.LamTerm i Bound -> Maybe Value
value (Lam.Val _ v ) = Just v
value _ = Nothing -- error $ show t ++ " is not a value"

-- | applys a build in function to one argument
--  It crashes if first argument is not a function (It only export to include int test)
applyValue :: Value -- ^ build in function
           -> Value -- ^ argument
           -> Value -- ^ result
applyValue BuildIn {arrity = 1, evaluator = e, stack = s } v = Strict.evalState e (v : s )
applyValue v1@BuildIn {arrity = n, stack = s, myType = t } v2 =
    v1 {arrity = n - 1 , stack = v2 : s, myType = dropTypeArg t}
applyValue v _ = error $ show v ++ " apply value"
