{-# LANGUAGE LambdaCase, TupleSections #-}
module MTable where

import BruijnTerm (BruijnTerm)
import BruijnEnvironment

-- TODO add comments

-- TODO add store
--      store should only be used if you only need info about current var
--      if you need to look up for different variables you should make your own env
--      to use own Env you have to track depth
--      so make it easy to get depth from mtable

-- TODO env rename name , get simbolTabe
data MTable = MTable
            { getDepth :: Int
            , incFreeFromStart :: Int
            , getEnv :: BruijnEnv (Int,Symbol ())
            } deriving (Eq, Show)

peekVar :: MTable -> Bound -> (Either Bound (BruijnTerm ()),MTable)
peekVar modifications b@(Bound n) =
  let table = getEnv modifications
  in case bMaybeLookup b table of
    Just (depthDefined, Undefined) -> (Left $ Bound $ depth- depthDefined - 1,modifications)
    Just (depthDefined, Subst t2) -> (Right t2, incFree (depth - depthDefined) empty)
    Nothing -> ( Left $ Bound $ n + incFreeFromStart modifications,modifications)
  where
    depth = getDepth modifications

data Symbol i = Subst (BruijnTerm i)
           | Undefined
           deriving (Show, Eq)

incFree :: Int -> MTable -> MTable
incFree n (MTable depth inc env) = MTable (depth +n) (inc+n) env

empty :: MTable
empty = MTable 0 0 bEmtyEnv

drop :: Int -> MTable -> MTable
drop n m = m{getDepth = getDepth m - n
            ,getEnv = bDrop n $ getEnv m}

reorder :: Int -> [Bound] -> MTable -> MTable
reorder n order s@MTable{getEnv = env} = s {getEnv = bReorder env n order}

substitute  :: Bound -> Int ->  BruijnTerm () -> MTable -> MTable
substitute (Bound n) depthDiff sub m =
 m{ incFreeFromStart = incFreeFromStart m -1
  , getEnv = bInsertAt n (getDepth m -depthDiff,Subst sub)(getEnv m)}

insertUndefined :: Int -> MTable -> MTable
insertUndefined n m@MTable{getDepth = depth} =
    m { getEnv = bInserts (map (,Undefined )[depth .. depth+n-1]) (getEnv m)
      , getDepth = depth + n}

--TODO remove or use in test for  incFreeFromStart == nsubst
-- nsubst :: BruijnEnv (Symbol i) -> Int
-- nsubst = bSize . bFilter (\ case
--                  Subst {} -> True
--                  _ -> False)
