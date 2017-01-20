{-# LANGUAGE LambdaCase#-}
module Eval
  ( eval
  , evalSteps
  , fullEval
  , applyValue
  -- , evalWithEnv
  )
where

import Control.Monad.Writer
import Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict as Strict
import Data.Maybe
import Data.Bifunctor

import BruijnTerm
import Value
import BruijnEnvironment
import Type
import Modify
import TaggedLambda as Tag
import LambdaF
import Lambda as Lam
import Unprocessed

type Evaluator a = WriterT [Unprocessed ()] (State (SymbolTable ())) a

trans :: Unprocessed ()
      -> (LamTermF () Bound (Unprocessed ()) -> Evaluator (Maybe (Unprocessed ())))
      -> Evaluator (Unprocessed ())
   --TODO make use maybe easyer
trans original f = fromMaybe original <$> go original
  where
    go :: Unprocessed () -> Evaluator (Maybe (Unprocessed ()))
    go (Unprocessed (Tag.Tag m t)) = localT $ do
       modify $ remember m
       newT <- censor (map $ addTag m) $ go (Unprocessed t)
       return (addTag m <$> newT)
    go term = peek term f

trans' :: Unprocessed ()
      -> (LamTermF () Bound (Unprocessed ()) -> State (SymbolTable ()) (Maybe (Unprocessed ())))
      -> State (SymbolTable ()) (Maybe (Unprocessed ()))
trans' original f = go original
  where
    go :: Unprocessed () -> State (SymbolTable ()) (Maybe (Unprocessed ()))
    go (Unprocessed (Tag.Tag m t)) = localT $ do
       modify $ remember m
       newT <- go (Unprocessed t)
       return (addTag m <$> newT)
    go term = peek term f

-- |eval term in accordance with call by value.
-- If a term can't be further be evaluated it will return 'Nothing'
eval :: BruijnTerm () -> Maybe (BruijnTerm ())
eval = listToMaybe . evalSteps

evalSteps :: BruijnTerm () -> [BruijnTerm ()]
evalSteps term = map (applyModify . getLambdaT) evaled
    where
    evaled = evalState (evalSteps' $ Unprocessed $ Tag.tag term) empty

fullEval :: BruijnTerm () -> BruijnTerm ()
fullEval term = applyModify $ getLambdaT evaled
  where
    evaled = evalState (fullEval' $ Unprocessed $ Tag.tag term) empty

fullEval' :: Unprocessed () -> State (SymbolTable ()) (Unprocessed ())
fullEval' orignal = fst <$> runWriterT (evalWithEnv orignal)

evalSteps' :: Unprocessed () -> State (SymbolTable ()) [Unprocessed ()]
evalSteps' orignal = execWriterT (evalWithEnv orignal)

produce :: Monad m => a -> WriterT [a] m a
produce a = tell [a] >> return a

evalWithEnv :: Unprocessed () -> WriterT [Unprocessed ()] (State (SymbolTable ())) (Unprocessed ())
evalWithEnv term = trans term $ \ case
    (ApplF t1 t2) -> do
        t2' <- censor (map $ appl t1) $ evalWithEnv t2
        t1' <- censor (map $ flip appl t2') $ evalWithEnv t1
        t2'' <- proces t2'
        result <- unrafel t1' t2''
        return $ Just result
      where
        unrafel function argument = trans function $ \ case
            (LetF _ defs subFunction) -> do
                bdefs <- mapM procesDef defs
                zipWithM_ store (defsBounds defs) $ map implementation bdefs
                t <- censor (map (mkLet () defs)) $ unrafel subFunction argument
                peek t $ \ case
                    (ValF i v) -> Just <$> produce ( val i v)
                    _ -> return $ Just $ mkLet () defs t
            f -> do
                final <- lift $ betaReduction f argument
                case final of
                      (Just body) -> tell [body] >> ( Just <$> evalWithEnv body)
                      Nothing -> return Nothing

    (PtrF _ _ t) -> Just <$> produce t
    (LetF _ defs t) -> do
        defs' <- retell (\ defW -> mkLet () defW t) $ evalDefs defs
        t' <- censor (map (mkLet () defs')) $ evalWithEnv t
        peek t' $ \ case
            (ValF i v) -> Just <$> produce ( val i v)
            _ -> return $ Just $ mkLet () defs' t'
    _ -> return Nothing

evalDefs :: [DefF () Bound (Unprocessed ())]
         -> WriterT [[DefF () Bound (Unprocessed ())]]
                 (State (SymbolTable ()))
                 [DefF () Bound (Unprocessed ())]
evalDefs defs = incrementalM evalDef (length defs - 1) defs
  where
    evalDef :: Int
            -> DefF () Bound (Unprocessed ())
            -> WriterT [DefF () Bound (Unprocessed ())]
                    (State (SymbolTable ()))
                    (DefF () Bound (Unprocessed ()), Int )
    evalDef b (DefF i n t_) = do
        result <- retell ( DefF i n ) $ evalWithEnv t_
        findResult <- proces result
        store (Bound b) findResult
        return (DefF i n result, b - 1)

incrementalM :: Monad m => (b -> a -> WriterT [a] m (a, b)) -> b -> [a] -> WriterT [[a]] m [a]
incrementalM _ _ [] = return []
incrementalM f b [a] = retell (: []) $ (: []) . fst <$> f b a
incrementalM f b (a : as) = do
    (newA, newState) <- retell (: as) $ f b a
    censorMap (newA :) ( incrementalM f newState as)

censorMap :: Monad m => (w -> w) -> WriterT [w] m w -> WriterT [w] m w
censorMap f = censor (map f) . fmap f

betaReduction :: LamTermF () Bound (Unprocessed ())
              -> BruijnTerm ()
              -> State (SymbolTable ()) (Maybe (Unprocessed ()))
betaReduction function argument = case function of
    LambdaF _ _ t -> do
        dropSymbol
        return $ Just $ sub argument t

    ValF _ v ->
        case value argument of
            Just v2 -> return (Just $ val () $ applyValue v v2)
            Nothing -> return Nothing

    LetF _ defs t -> do
        bdefs <- mapM procesDef defs
        zipWithM_ store (defsBounds defs) $ map implementation bdefs
        fmap (mkLet () defs) <$> trans' t (flip betaReduction argument)

    _ -> return Nothing

retell :: Monad m => (w -> w') -> WriterT [w] m a -> WriterT [w'] m a
retell f = mapWriterT (fmap $ second (map f))

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
applyValue _ _ = error "apply value"
