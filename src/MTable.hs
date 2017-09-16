module MTable
  ( MTable(_env)
  , Symbol (Undefined) -- TODO these 2 are export for test but should not be used
  , peekVar
  , incFree
  , mTable
  , empty
  , reorder
  , MTable.drop
  , substitute
  , extraSparceInsertUndefind
  , insertUndefined
  )
where

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

data MTable = MTable
            { _depth :: Int
            , _incFreeFromStart :: Int
            , _env :: BruijnEnv (Int,Symbol)
            } deriving (Eq, Show)

peekVar :: MTable -> Bound -> (Either Bound (BruijnTerm ()),MTable)
peekVar modifications b@(Bound n) =
  case getLevel b table of
    Just (level ,Undefined) -> (Left $ Bound $ depth - level -1,modifications)
    Just (level ,Subst t) -> (Right t, mTable depth (depth - level))
    Nothing -> (Left $ Bound $ n + _incFreeFromStart modifications,modifications)
  where
    depth = _depth modifications
    table = _env modifications

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
empty = mTable 0 0

mTable :: Int -> Int -> MTable
mTable depth inc = MTable depth inc bEmtyEnv

drop :: Int -> MTable -> MTable
drop n m = m{_depth = _depth m - n
            ,_env = bDrop n $ _env m}

reorder :: Int -> [Bound] -> MTable ->MTable
reorder n order s@ MTable{_env=env} = s {_env=newEnv}
  where
    newEnv  = foldl go env $ zip order [n ..]
    go envN (bi, j) = case getLevel bi env of
      Just (level,sym ) -> bReplace (Bound j) (level,sym) envN
      Nothing -> error "cant reorder what is not there"

substitute  :: Bound -> Int ->  BruijnTerm () -> MTable -> MTable
substitute (Bound n) depthDiff sub m =
 m{ _incFreeFromStart = _incFreeFromStart m -1
  -- TODO does bInsertAt n  work if it is past a subscription or incfree
  , _env = bInsertAt n (depth -depthDiff ,Subst sub)(_env m)}
  where
    depth = _depth m

extraSparceInsertUndefind :: Int -> MTable -> MTable
extraSparceInsertUndefind n m@MTable{_depth = depth,_env=env} = case getLevel (Bound 0) env of
  Just (level, _) | level == depth - 1-> m {_depth = depth + n, _env = bInsertBlackhole n env}
  _ -> insertUndefined n m

insertUndefined :: Int -> MTable -> MTable
insertUndefined 0 m = m
insertUndefined n m@MTable{_depth = depth,_env=env} = assert (n >= 1)
    m { _env = bInsertBlackhole (n-1 ) $ bInsert (depth, Undefined ) env
      , _depth = depth + n}
