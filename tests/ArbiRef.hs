module ArbiRef where

import qualified Data.IntMap as IM

import Logic
import GenState

import BruijnEnvironment
import FreeEnvironment
import Name

class ArbiRef n where
    updateState :: GenState n -> Bool -> String -> Free -> GenState n
    refFromState :: GenState n -> Generater (n, Free)

instance ArbiRef Bound where
    updateState = updateStateBound
    refFromState = boundFromState

instance ArbiRef Name where
    updateState = updateStateName
    refFromState = nameFromState

-- TODO decouple bruijnMap
boundFromState :: GenState Bound -> Generater (Bound, Free )
boundFromState s = do
   (i, (_, f)) <- elementsLogic $ bToList $ tEnv s
   return (Bound (bruijnDepth (tEnv s ) - i - 1), f)

updateStateBound :: GenState n -> Bool -> String -> Free -> GenState n
updateStateBound state _ name free = state {tEnv = newTEnv}
    where newTEnv = bInsert (name, free) (tEnv state)

nameFromState :: GenState Name -> Generater (Name, Free )
nameFromState s = do
   (_, (name, f)) <- elementsLogic $ bToList $ tEnv s
   return (Name name, f)

-- TODO remove bool always remove name from list?
updateStateName :: GenState n -> Bool -> String -> Free -> GenState n
updateStateName state@State {tEnv = env } newVar name free = state {tEnv = newTEnv}
    where newTEnv :: BruijnEnv (String, Free)
          newTEnv = bInsert (name, free) (if newVar
            then removeVar name env
            else env)

-- TODO decouple bruijnMap
removeVar :: String -> BruijnEnv (String, b) -> BruijnEnv (String, b)
removeVar varname env = env {bruijnMap = fst $ IM.partition (\ var -> fst var == varname) (bruijnMap env)}
