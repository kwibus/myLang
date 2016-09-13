module Generator where

import Data.Maybe
import Test.QuickCheck.Gen
import Control.Monad.State.Class
import Control.Monad
import SearchTree

import Logic
import FreeEnvironment
import Type
import TypeCheck
import ErrorCollector
import Control.Monad.State.Strict

type Generater a = LogicGen (Env, Int) SearchTree a
type Env = FreeEnv Type

runGenerartor ::Generater a -> Gen (Maybe a)
runGenerartor g = fmap listToMaybe $ pruneT 100 5 $ evalStateT g (fEmtyEnv, 0)

unifyGen :: Maybe Type -> Type -> Generater ()
unifyGen Nothing _ = return ()
unifyGen (Just t1) t2 = do
    sub <- getSub
    case unify (apply sub t1)(apply sub t2) >>= unifySubs sub of
        Error {} -> mzero
        (Result sub1) -> setEnv sub1

getSub :: Generater TSubst
getSub = liftM fst get

setEnv :: FreeEnv Type -> Generater ()
setEnv env = modify (\ (_, m) -> (env, m))

newFreeVar :: Generater Free
newFreeVar = do
    (env, i) <- get
    put (env , i + 1)
    return $ Free i

getMax :: Generater Int
getMax = do
    (_, i) <- get
    return i

typeSizeBigger :: Int -> Type -> Generater Bool
typeSizeBigger i t = do
    sub <- getSub
    return $ i < typeSize ( apply sub t)
