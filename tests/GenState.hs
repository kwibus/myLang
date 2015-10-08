module GenState where

import Data.Maybe
import Test.QuickCheck.Gen
import Control.Monad.State.Class
import Control.Monad

import Logic
import Name
import Environment
import Type
import TypeCheck

data GenState n = State
  { freeNames :: [String]
  , dictionary :: BruiEnv (String, Free)
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
    case unify t1 t2 env of
        Left {} -> mzero
        Right env1 -> setEnv env1

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

-- TODO Size capital S
typesizeBigger :: Int -> Type -> Generater Bool
typesizeBigger i t = do
    env <- getEnv
    return $ i < size ( apply t env )
