{-# LANGUAGE LambdaCase#-}
module Eval
  ( eval
  , evalSteps
  , fullEval
  , applyValue
  , evalWithEnv
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

-- |eval term in accordance with call by value.
-- If a term can't be further be evaluated it will return 'Nothing'
eval :: BruijnTerm () -> Maybe (BruijnTerm ())
eval = listToMaybe . evalSteps

evalSteps :: BruijnTerm () -> [BruijnTerm ()]
evalSteps term = map (applyModify . getLambdaT) evaled
    where
    evaled = snd $ runEval term

fullEval :: BruijnTerm () -> BruijnTerm ()
fullEval = applyModify . getLambdaT . fst . runEval

runEval :: BruijnTerm () -> (Unprocessed (), [Unprocessed ()])
runEval term = evalState (runWriterT $ evalWithEnv unprocessed) empty
  where
    unprocessed = Unprocessed $ Tag.tag term

evalWithEnv :: Unprocessed () -> Evaluator (Unprocessed ())
evalWithEnv term = trans term $ \ case
    (ApplF t1 t2) -> do
        t2' <- censor (map $ appl t1) $ evalWithEnv t2
        t1' <- censor (map $ flip appl t2') $ evalWithEnv t1
        t2'' <- proces t2'
        Just <$> unrafel t1' t2''
        where
          unrafel :: Unprocessed () -> BruijnTerm () -> Evaluator (Unprocessed ())
          unrafel function argument =
            trans function $ \ case
                LambdaF _ _ t1'' -> do
                    tell [sub argument t1'']
                    substitut (Bound 0) argument
                    censor (map (sub argument)) $ Just . sub argument <$> evalWithEnv t1''

                ValF _ v ->
                    case value argument of
                        Just v2 ->
                            let newvalue = val () $ applyValue v v2
                            in tell [ newvalue ] >> return ( Just newvalue)
                        Nothing -> return Nothing

                LetF _ defs t -> do
                    bdefs <- mapM procesDef defs
                    zipWithM_ store (defsBounds defs) $ map implementation bdefs
                    result <- censor (map $ mkLet () defs) $ unrafel t argument
                    peek result $ \ case
                        (ValF _ v) -> tell [val () v] >> return (Just $ val () v)
                        _ -> return $ Just $ mkLet () defs result

                t -> do
                        env <- get
                        return $ error $ show (t, term, env)
    LetF _ defs t -> do
        defs' <- retell (\ defW -> mkLet () defW t ) $ evalDefs defs
        t' <- censor ( map ( mkLet () defs' )) $ evalWithEnv t
        peek t' $ \ case
            ValF i v ->
                let v' = val i v
                in tell [v'] >> return (Just v')
            _ -> return $ Just $ mkLet () defs' t'

    (PtrF _ _ foundTerm) -> do
        tell [foundTerm]
        return $ Just foundTerm

    _ -> return Nothing

evalDefs :: [DefF () Bound (Unprocessed ())] -> WriterT [[DefF () Bound (Unprocessed ())]] (State (SymbolTable ())) [DefF () Bound (Unprocessed ())]
evalDefs defs = do
    let go n evaledDefs unevaledDefs = case unevaledDefs of
            [] -> error "ice let without def"
            [lasDef] -> (: []) <$> censorDef n evaledDefs lasDef
            (currentDef : nextDefs) ->
                do evalCurrentDef <- censorDef n evaledDefs currentDef
                   (:) evalCurrentDef <$> go (n + 1) ( evaledDefs ++ [evalCurrentDef]) nextDefs
    go 0 [] defs
  where
    nDefs = length defs
    censorDef :: Int
              -> [DefF () Bound (Unprocessed ())]
              -> DefF () Bound (Unprocessed ())
              -> WriterT [[DefF () Bound (Unprocessed ())]] (State ( SymbolTable ())) (DefF () Bound (Unprocessed ()))
    censorDef n evaledDefs def = retell
        (\ currentWriten -> evaledDefs ++ [currentWriten] ++ drop (n + 1) defs) $
        evalDef (Bound (nDefs - n - 1), def)

    evalDef :: (Bound, DefF () Bound (Unprocessed ()))
            -> WriterT [DefF () Bound (Unprocessed () )] (State (SymbolTable ())) (DefF () Bound (Unprocessed ()) )
    evalDef (b, DefF i n t_) = do
        result <- retell ( DefF i n ) $ evalWithEnv t_
        finResult <- proces result
        store b finResult
        return (DefF i n result)

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
