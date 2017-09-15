{-# LANGUAGE LambdaCase #-}
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
            , getEnv :: BruijnEnv (Symbol ())
            } deriving (Eq, Show)

peekVar :: MTable -> Bound -> (Either Bound (BruijnTerm ()),MTable)
peekVar modifications b@(Bound n) =
  let table = getEnv modifications
  in case bMaybeLookup b table of
    Just (Undefined depthDefined) -> (Left $ Bound $ depth- depthDefined - 1,modifications)
    Just (Subst depthDefined t2) -> (Right t2, incFree (depth - depthDefined) empty)
    Nothing -> ( Left $ Bound $ n + incFreeFromStart modifications,modifications)
  where
    depth = getDepth modifications

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

insertUndefined :: Int -> MTable -> MTable
insertUndefined n m = m { getEnv = bInserts (map Undefined  [depth .. depth+n-1]) (getEnv m)
                        , getDepth = depth + n}
  where
    depth = getDepth m

--TODO remove or use in test for  incFreeFromStart == nsubst
-- nsubst :: BruijnEnv (Symbol i) -> Int
-- nsubst = bSize . bFilter (\ case
--                  Subst {} -> True
--                  _ -> False)
