module GenState where

import Data.Maybe
import Test.QuickCheck.Gen
import Control.Monad.State.Class
import Control.Monad

import Logic
import Name
import FreeEnvironment
import BruijnEnvironment
import Type
import TypeCheck
import Control.Monad.State.Strict
import SearchTree

data GenState n = State
  { freeNames :: [String]
  , tEnv :: BruijnEnv (String, Free)
  } deriving Show

-- TODO rename defualtgenstate
defualtGenState :: GenState n
defualtGenState = State
  { tEnv = bEmtyEnv
  , freeNames = letters
  }


disableFromEnv :: Bound -> GenState n -> GenState n
disableFromEnv b s@State {tEnv = env } = s{tEnv = bDelete b env}

type Generater a = LogicGen (Env, Int) SearchTree a
type Env = FreeEnv Type

runGenerartor :: Generater a -> SearchTree Gen a
runGenerartor g = evalStateT g (fEmtyEnv, 0)

generateGen :: Generater a -> Gen (Maybe a)
generateGen g = listToMaybe <$> pruneT 200 ( runGenerartor g)

generateList :: Generater a -> Gen [a]
generateList = foundT . runGenerartor

unifyGen :: Maybe Type -> Type -> Generater ()
unifyGen Nothing _ = return ()
unifyGen (Just t1) t2 = do
    sub <- getSub
    case unify (apply sub t1) (apply sub t2) >>= unifySubs sub of
        Left {} -> mzero
        (Right sub1) -> setEnv sub1

getSub :: Generater (TSubst Type)
getSub = liftM fst get

setEnv :: FreeEnv Type -> Generater ()
setEnv env = modify (\ (_, m) -> (env, m))

newFreeVar :: Generater Free
newFreeVar = do
    (env, i) <- get
    put (env , i + 1)
    return $ Free i


--TODO remove
getMax :: Generater Int
getMax = do
    (_, i) <- get
    return i

typeSizeBigger :: Int -> Type -> Generater Bool
typeSizeBigger i t = do
    sub <- getSub
    return $ i < typeSize ( apply sub t)
