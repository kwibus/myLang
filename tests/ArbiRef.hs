module ArbiRef where

import qualified Data.IntMap as IM

import Logic
import GenState

import Environment
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

boundFromState :: GenState Bound -> Generater (Bound, Free )
boundFromState s = do
   (i, (_, f)) <- elementsLogic $ bToList $ dictionary s
   return (Bound (bruiDepth (dictionary s ) - i - 1), f)

updateStateBound :: GenState n -> Bool -> String -> Free -> GenState n
updateStateBound state _ name free = state {dictionary = newdic}
    where newdic = bInsert (name, free) (dictionary state)

nameFromState :: GenState Name -> Generater (Name, Free )
nameFromState s = do
   (_, (name, f)) <- elementsLogic $ bToList $ dictionary s
   return (Name name, f)

updateStateName :: GenState n -> Bool -> String -> Free -> GenState n
updateStateName state@State {dictionary = dic} newVar name free = state {dictionary = newDic}
    where newDic :: BruiEnv (String, Free)
          newDic = bInsert (name, free) (if newVar
            then removeVar name dic
            else dic)

removeVar :: String -> BruiEnv (String, b) -> BruiEnv (String, b)
removeVar varname env = env {bruiMap = fst $ IM.partition (\ var -> fst var == varname) (bruiMap env)}
