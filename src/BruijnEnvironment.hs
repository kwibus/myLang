module BruijnEnvironment where

import qualified Data.IntMap as IM
import Control.Exception.Base
import Data.Maybe
import Data.List

-- add some test
-- TODO remove b prefix and give beter names

-- TODO consistend inuative order in BruijnEnv
--   defs / bfromlist is [2,1,0]
--   Reorder ?

-- | Bound is wrapper arround Int and is used to represent BruijnIndex.
-- BruijnIndex rever to a env, but are dependent on the surrounding terms.
-- If you add extra lambda:
--
-- * \\a.a  ==> \\a.\\b.a
--
-- * \\0  ==>   \\1
--
-- You have to modify the Inde
newtype Bound = Bound Int deriving (Eq, Show, Ord)

--TODO replace with list
--TODO Fix name to BruijnEnv
data BruijnEnv a = BruijnState
     { bruijnDepth :: Int
     , bruijnMap :: IM.IntMap a
     } deriving Eq

-- TODO maybe import to debug module
instance Show a => Show (BruijnEnv a) where
    show env = '[' :
        intercalate "," ( map showindex $ Bound <$> fromToZero (depth - 1))
        ++ "<" ++ show depth ++ "]"
      where
        depth = bruijnDepth env
        showindex i = fromMaybe "_" $ show <$> bMaybeLookup i env

fromToZero :: Int -> [Int]
fromToZero n | n < 0 = []
             | otherwise = n : fromToZero (pred n)

toInt :: Bound -> Int
toInt (Bound i) = i

bNull :: BruijnEnv a -> Bool
bNull BruijnState {bruijnDepth = 0} = True
bNull _ = False

bEmtyEnv :: BruijnEnv a
bEmtyEnv = BruijnState --TODO rename
    { bruijnDepth = 0
    , bruijnMap = IM.empty
    }
bMember :: Bound -> BruijnEnv a -> Bool
bMember b e = isJust $ bMaybeLookup b e

bLookup :: Bound -> BruijnEnv a -> a
bLookup (Bound i) BruijnState {bruijnDepth = depth, bruijnMap = m} =
    m IM.! (depth - i - 1)

bMaybeLookup :: Bound -> BruijnEnv a -> Maybe a
bMaybeLookup (Bound i) BruijnState {bruijnDepth = depth, bruijnMap = m} =
    IM.lookup (depth - i - 1) m

bLookupLT :: Bound -> BruijnEnv a -> Maybe (Bound, a)
bLookupLT (Bound i) BruijnState {bruijnDepth = depth, bruijnMap = m} =
    case IM.lookupLE (depth -i -1) m of
      Just (ik,a ) -> Just (Bound (depth-1-ik),a)
      Nothing -> Nothing

bInsert :: a -> BruijnEnv a -> BruijnEnv a
bInsert a b@BruijnState {bruijnDepth = depth, bruijnMap = m} =
     b {bruijnDepth = depth + 1, bruijnMap = IM.insert depth a m }

-- TODO test (bInserts [a] == bInsert a)
-- when env= bInserts [1,2,3] bEmtyEnv  then bLookup Bound 0 will be 3; Bruij counts left to righ
bInserts :: [a] -> BruijnEnv a -> BruijnEnv a
bInserts list env = foldl' (flip bInsert) env list

bInsertBlackhole :: Int  -> BruijnEnv a -> BruijnEnv a
bInsertBlackhole n env =
  assert (n >= 0) $
  env{bruijnDepth = bruijnDepth env +n}

-- TODO can remove duplcate code by using bInserts
bFromList :: [a] -> BruijnEnv a
bFromList = foldl' (flip bInsert) bEmtyEnv

-- TODO remove this
bToList :: BruijnEnv a -> [(Int, a)]
bToList BruijnState {bruijnMap = m} = IM.toList m

bReplace ::  Bound -> a -> BruijnEnv a -> BruijnEnv a
bReplace (Bound i) a b@BruijnState {bruijnDepth = depth, bruijnMap = m} =
  assert (i < depth) $
  b {bruijnMap = IM.insert (depth - i - 1 ) a m}

bDelete :: Bound -> BruijnEnv a -> BruijnEnv a
bDelete (Bound i) b@BruijnState {bruijnDepth = depth, bruijnMap = m} =
  b {bruijnMap = IM.delete(depth - i - 1 ) m}

-- TODO ??? could remove duplecate cate by using bSplitAt
bDrop :: Int -> BruijnEnv a -> BruijnEnv a
bDrop n b = assert (n >= 0 && n <= bruijnDepth b) b {bruijnDepth = newDepth, bruijnMap = newM}
    where (newM, _) = IM.split newDepth (bruijnMap b)
          newDepth = bruijnDepth b - n

bExtend :: Int -> BruijnEnv a -> BruijnEnv a
bExtend n = bDrop (-n)

bFilter :: (a -> Bool) -> BruijnEnv a -> BruijnEnv a
bFilter f env = env {bruijnMap = IM.filter f $ bruijnMap env}

bSize :: BruijnEnv a -> Int
bSize = IM.size . bruijnMap

bSplitAt :: Int -> BruijnEnv a -> (BruijnEnv a, [a])
bSplitAt n b = (b {bruijnDepth = newDepth, bruijnMap = low}, maybeToList pivot ++ map snd (IM.toAscList high))
  where (low, pivot, high) = IM.splitLookup newDepth (bruijnMap b)
        newDepth = bruijnDepth b - n

-- TODO test (bInsertAt 0  == bInsert)
-- TODO maybe use Bound
bInsertAt :: Int -> a -> BruijnEnv a -> BruijnEnv a
bInsertAt n a env = bInserts (a:right) left
  where
       (left,right) = bSplitAt n env

instance Functor BruijnEnv where
    fmap f b = b {bruijnMap = fmap f (bruijnMap b)}

instance Traversable BruijnEnv where
  traverse f (BruijnState dept intmap) = BruijnState dept <$> traverse f intmap

instance Foldable BruijnEnv where
  foldr f b (BruijnState _ intmap) = foldr f b intmap

mapWithBound :: (Bound -> a -> b) -> BruijnEnv a -> BruijnEnv b
mapWithBound f b@BruijnState {bruijnDepth = dept, bruijnMap = m} =
    b {bruijnMap = IM.mapWithKey (\ index a -> f (Bound $! dept - index - 1) a) m}

-- TODO add comments
bReorder :: BruijnEnv a -> Int  -> [Bound] -> BruijnEnv a
bReorder env n order = foldl go env $ zip order [n ..]
  where go envN (bi , j) = bReplace (Bound j) (bLookup bi env) envN
