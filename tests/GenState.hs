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
import ErrorCollector

data GenState n = State
  { freeNames :: [String]
  , dictionary :: BruijnEnv (String, Free)
  } deriving Show

-- TODO rename defualtgenstate
defualtGenState :: GenState n
defualtGenState = State
  { dictionary = bEmtyEnv
  , freeNames = letters
  }

type Generater a = LogicGen (Env, Int) a
type Env = FreeEnv Type

runGenerartor :: Generater a -> Gen (Maybe a)
runGenerartor g = fmap listToMaybe (evalT g (fEmtyEnv, 0))

unifyGen :: Maybe Type -> Type -> Generater ()
unifyGen Nothing _ = return ()
unifyGen (Just t1) t2 = do
    env <- getEnv
    case unify (apply t1 env)(apply  t2 env) >>= unifyEnv  env of
        Error {} -> mzero
        (Result env1) -> setEnv env1

getEnv :: Generater (FreeEnv Type)
getEnv = liftM fst get

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
    env <- getEnv
    return $ i < typeSize ( apply t env )
