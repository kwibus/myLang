{-# LANGUAGE UndecidableInstances, FlexibleInstances,MultiParamTypeClasses #-}
module GenState where

import Names
import Enviroment
import Type
import TypeCheck
import Control.Monad.State.Lazy
import Test.QuickCheck.Gen
import Data.Maybe
import StateTransMany

data GenState = State
    { freeNames :: [String]
    , dictionary :: BruiEnv (String, ((), Free))
    }

defualtGenState :: GenState
defualtGenState = State { dictionary = bEmtyEnv
                        , freeNames = letters
                        }

type Generater a =  StateTransManyT (Env,Int) Gen a
type Env = FreeEnv (Type Free)

runGenerartor :: Generater a ->  Gen (Maybe a)
runGenerartor g =  fmap listToMaybe  (evalT g (fEmtyEnv,0))

unifyGen :: Type Free-> Type Free -> Generater ()
unifyGen t1 t2 = do
    env <- getEnv 
    case unify t1 t2 env of
        Left {} -> mzero
        Right env1 -> setEnv env1 

getEnv :: Generater (FreeEnv (Type Free))
getEnv = get >>= return . fst

setEnv :: FreeEnv (Type Free )->Generater ()
setEnv env = modify  (\(_,m) -> (env,m))

newFreeVar :: Generater Free
newFreeVar = do
    (env, i) <- get
    put (env ,i+1) 
    return $ Free i

getMax :: Generater Int 
getMax = do
    (_, i) <- get
    return i

typesizeSmaller:: Int ->Type Free ->Generater Bool
typesizeSmaller i t= do 
    env <- getEnv
    return $ i < tSize ( apply t env )


