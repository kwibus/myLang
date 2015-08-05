{-# LANGUAGE UndecidableInstances, FlexibleInstances,MultiParamTypeClasses #-}
module GenState where

import Data.Maybe
import Control.Monad.State.Lazy
import Test.QuickCheck.Gen

import StateTransMany

import Name
import Environment
import Type
import TypeCheck

data GenState n = State
  { freeNames :: [String]
  , dictionary :: BruiEnv (String, Free)
  }

defualtGenState :: GenState n
defualtGenState = State
  { dictionary = bEmtyEnv
  , freeNames = letters
  }

type Generater a = StateTransManyT (Env, Int) Gen a
type Env = FreeEnv (Type Free)

runGenerartor :: Generater a -> Gen (Maybe a)
runGenerartor g = fmap listToMaybe (evalT g (fEmtyEnv, 0))

unifyGen :: Maybe (Type Free) -> Type Free -> Generater ()
unifyGen Nothing _ = return ()
unifyGen (Just t1) t2 = do
    env <- getEnv
    case unify t1 t2 env of
        Left {} -> mzero
        Right env1 -> setEnv env1

getEnv :: Generater (FreeEnv (Type Free))
getEnv = liftM fst get

setEnv :: FreeEnv (Type Free ) -> Generater ()
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

typesizeBigger :: Int -> Type Free -> Generater Bool
typesizeBigger i t = do
    env <- getEnv
    return $ i < tSize ( apply t env )
