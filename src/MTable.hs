{-# LANGUAGE LambdaCase #-}
module MTable where

import Control.Exception.Base
import BruijnTerm (BruijnTerm)
import BruijnEnvironment

-- TODO add comments

-- TODO add store
--      store should only be used if you only need info about current var
--      if you need to look up for different variables you should make your own env
--      to use own Env you have to track depth
--      so make it easy to get depth from mtable
--
-- TODO levels range is [0.. depth-1]  maybe is easyer if it was [1..depth]
--
-- TODO env rename name
data MTable = MTable
            { getDepth :: Int
            , incFreeFromStart :: Int
            , getEnv :: BruijnEnv (Int,Symbol)
            } deriving (Eq, Show)

peekVar :: MTable -> Bound -> (Either Bound (BruijnTerm ()),MTable)
peekVar modifications b@(Bound n) =
  let table = getEnv modifications
  in case getLevel b table of
    Just (level ,Undefined) -> (Left $ Bound $ depth - level -1,modifications)
    Just (level ,Subst t) -> (Right t, incFree (depth - level) empty)
    Nothing -> (Left $ Bound $ n + incFreeFromStart modifications,modifications)
  where
    depth = getDepth modifications

getLevel :: Bound -> BruijnEnv (Int,Symbol)-> Maybe (Int,Symbol)
getLevel b@(Bound n) env =
  case bLookupLT b env of
    Just (Bound nFound ,(levelFound,sym))
        | nFound == n -> Just (levelFound, sym)
        | otherwise -> Just (levelFound + (nFound-n),Undefined)
    Nothing -> Nothing

data Symbol = Subst (BruijnTerm ())
           | Undefined
           deriving (Show, Eq)

incFree :: Int -> MTable -> MTable
incFree n (MTable depth inc env) = MTable (depth +n) (inc+n) env

empty :: MTable
empty = MTable 0 0 bEmtyEnv

drop :: Int -> MTable -> MTable
drop n m = m{getDepth = getDepth m - n
            ,getEnv = bDrop n $ getEnv m}

reorder :: Int -> [Bound] -> MTable ->MTable
reorder n order s@ MTable{getEnv=env} = s {getEnv=newEnv}
  where
    newEnv  = foldl go env $ zip order [n ..]
    go envN (bi, j) = case getLevel bi env of
      Just (level,sym ) -> bReplace (Bound j) (level,sym) envN
      Nothing -> error "cant reorder what is not there"

substitute  :: Bound -> Int ->  BruijnTerm () -> MTable -> MTable
substitute (Bound n) depthDiff sub m =
 m{ incFreeFromStart = incFreeFromStart m -1
  , getEnv = bInsertAt n (depth -depthDiff ,Subst sub)(getEnv m)}
  where
    depth = getDepth m

extraSparceInsertUndefind :: Int -> MTable -> MTable
extraSparceInsertUndefind n m@MTable{getDepth = depth,getEnv=env} = case getLevel (Bound 0) env of
  Just (level, _) | level == depth - 1-> m {getDepth = depth + n, getEnv = bInsertBlackhole n env}
  _ -> insertUndefined n m

insertUndefined :: Int -> MTable -> MTable
insertUndefined 0 m = m
insertUndefined n m@MTable{getDepth = depth,getEnv=env} = assert (n >= 1)
    m { getEnv = bInsertBlackhole (n-1 ) $ bInsert (depth, Undefined ) env
      , getDepth = depth + n}
