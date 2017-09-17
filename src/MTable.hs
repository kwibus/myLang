-- | this module defines a way to store/delaye modifications

module MTable
  ( MTable(_env)
  , Symbol (Undefined) -- TODO these 2 are export for test but should not be used
  , peekVar
  , incFree
  , mTable
  , empty
  , isNull
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
--
-- TODO maybe parameterise over symbol
--      then substitute becomes (store (Subst t) $ incFree (-1))
--
-- TODO levels range is [0.. depth-1]  maybe is easyer if it was [1..depth]
--
-- | 'MTable' is data structure to store delayed modifications on ast  that use Bruijn indeces
--
-- suported modifications:
--
-- * 'incFree'
--
-- * 'reorder'
--
-- * 'substitute'
--
-- * and potentialy storing meta information variable
--
data MTable = MTable
            { _depth :: Int
            , _incFreeFromStart :: Int
            , _env :: BruijnEnv (Int,Symbol)
            } deriving (Eq, Show)

-- | peekVar will query mtable with Bruijn index and answer with new Bruijn index to replace it with
-- or (new 'BruijnTerm' with  'MTable' with delayed modifications on that ast). see substitute for why
--
-- this is a low level interface, you should probably use 'Modify.peek' or 'ModificationTags.peek'
peekVar :: MTable -> Bound -> Either Bound (BruijnTerm (),MTable)
peekVar modifications b@(Bound n) =
  case getLevel b table of
    Just (level ,Undefined) -> Left $ Bound $ depth - level -1
    Just (level ,Subst t) -> Right (t, mTable depth (depth - level))
    Nothing -> Left $ Bound $ n + _incFreeFromStart modifications
  where
    depth = _depth modifications
    table = _env modifications

-- |
-- * Bruijn levels start from 0
--
-- * BruijnEnv is sparse. So when index is not found it calculated level from what is stored below
-- it will asume Undefined for it's symbol
getLevel :: Bound -- ^ Bruijn index to query
         -> BruijnEnv (Int,Symbol) -- ^ sparse store for (levels,symbols)
         -> Maybe (Int,Symbol) -- ^ 'Nothing' if it was not bound /'Just' (bruijn Level, symbol)
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

-- | check if there are no modifications stored. This can say there are while there are realy none
isNull :: MTable -> Bool
isNull (MTable _ 0 env) | env == bEmtyEnv = True
isNull _ = False

mTable :: Int -- ^ depth of corresponding BruijnTerm
       -> Int -- ^ incFree
       -> MTable -- ^ new 'MTable'
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

-- TODO rename/and make defaut
-- TODO test
extraSparceInsertUndefind :: Int -> MTable -> MTable
extraSparceInsertUndefind n m@MTable{_depth = depth,_env=env} = case getLevel (Bound 0) env of
  Just (level, _) | level == depth - 1-> m {_depth = depth + n, _env = bInsertBlackhole n env}
  _ -> insertUndefined n m

-- TODO dont export
insertUndefined :: Int -> MTable -> MTable
insertUndefined 0 m = m
insertUndefined n m@MTable{_depth = depth,_env=env} = assert (n >= 1)
    m { _env = bInsertBlackhole (n-1 ) $ bInsert (depth, Undefined ) env
      , _depth = depth + n}
