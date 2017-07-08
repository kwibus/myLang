{-# LANGUAGE LambdaCase #-}
module MTable where


import BruijnTerm (BruijnTerm)
import BruijnEnvironment
-- TODO maybe make a mtable separte module rename

-- TODO env rename name , get simbolTabe
data MTable = MTable
            { getDepth :: Int
            , getEnv :: BruijnEnv (Symbol ())}
              deriving (Eq, Show)

data Symbol i = Subst Int (BruijnTerm i)
           | Undefined Int
           deriving (Show, Eq)

empty :: MTable
empty = MTable 0 bEmtyEnv

drop :: Int -> MTable -> MTable
drop n m = m{getDepth = getDepth m - n
          ,getEnv = bDrop n $ getEnv m}

-- TODO rename
insertT :: [Symbol ()] -> MTable -> MTable
insertT refs s@MTable {getEnv = env, getDepth = depth}
    = s {getEnv = bInserts refs env
        , getDepth = depth + length refs}

--TODO remove
nsubst :: BruijnEnv (Symbol i) -> Int
nsubst = bSize . bFilter (\ case
                 Subst {} -> True
                 _ -> False)
