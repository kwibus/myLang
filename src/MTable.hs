{-# LANGUAGE LambdaCase #-}
module MTable where

import BruijnTerm (BruijnTerm)
import BruijnEnvironment

-- TODO add comments
-- TODO add store

-- TODO env rename name , get simbolTabe
data MTable = MTable
            { getDepth :: Int
            , incFreeFromStart :: Int
            , getEnv :: BruijnEnv (Symbol ())
            }
              deriving (Eq, Show)

data Symbol i = Subst Int (BruijnTerm i)
           | Undefined Int
           deriving (Show, Eq)

incFree :: Int -> MTable -> MTable
incFree n (MTable depth inc env) = MTable (depth +n) (inc+n) env

empty :: MTable
empty = MTable 0 0 bEmtyEnv

drop :: Int -> MTable -> MTable
drop n m = m{getDepth = getDepth m - n
            ,getEnv = bDrop n $ getEnv m}

substitute  :: Bound -> Int ->  BruijnTerm () -> MTable -> MTable
substitute (Bound n) depthDiff sub m =
 m{ incFreeFromStart = incFreeFromStart m -1
  , getEnv = bInsertAt n (Subst (getDepth m -depthDiff ) sub )(getEnv m)}

-- TODO rename/remove
insertT :: [Symbol ()] -> MTable -> MTable
insertT refs s@MTable {getEnv = env, getDepth = depth}
    = s {getEnv = bInserts refs env
        , getDepth = depth + length refs}

--TODO remove or use in test for  incFreeFromStart == nsubst
-- nsubst :: BruijnEnv (Symbol i) -> Int
-- nsubst = bSize . bFilter (\ case
--                  Subst {} -> True
--                  _ -> False)
