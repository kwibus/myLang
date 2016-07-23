module BruijnEnvironment where

import qualified Data.IntMap as IM
import Control.Exception.Base
import Data.Maybe
import Data.List
-- TODO remove b prefix
-- | Bound is wrapper arround Int and is used to represent BruijnIndex.
-- BruijnIndex rever to a env, but are dependent on the surrounding terms.
-- If you add extra lambda:
--
-- * \\a.a  ==> \\a.\\b.a
--
-- * \\0  ==>   \\1
--
-- You have to modify the Inde
newtype Bound = Bound Int deriving (Eq, Show)

--TODO replace with list
--TODO Fix name to BruijnEnv
type BruijnEnv a = IM.IntMap a 


toInt :: Bound -> Int
toInt (Bound i) = i

bNull :: BruijnEnv a -> Bool
bNull = IM.null

bEmtyEnv :: BruijnEnv a
bEmtyEnv = IM.empty

bMember :: Bound -> BruijnEnv a -> Bool
bMember (Bound b) = IM.member b

bLookup :: Bound -> BruijnEnv a -> a
bLookup (Bound i) env =
    env IM.! i

bMaybeLookup :: Bound -> BruijnEnv a -> Maybe a
bMaybeLookup (Bound i) = IM.lookup i

bInsert :: Bound -> a -> BruijnEnv a -> BruijnEnv a
bInsert (Bound i) = IM.insert i

bInserts :: [a] ->  BruijnEnv a -> BruijnEnv a
bInserts list env = undefined  -- foldl' (flip bInsert) env list
--
-- -- TODO can remove duplcate code by using bInserts
bFromList :: [a] -> BruijnEnv a
bFromList = undefined --  foldl' (flip bInsert) bEmtyEnv
--
-- -- TODO remove this
-- bToList :: BruijnEnv a -> [(Int, a)]
-- bToList BruijnState {bruijnMap = m} = IM.toList m
--
-- bReplace :: Bound -> a -> BruijnEnv a -> BruijnEnv a
-- bReplace (Bound i) a b@BruijnState {bruijnDepth = depth, bruijnMap = m} =
--   assert (bMember (Bound i) b)
--   b {bruijnMap = IM.insert (depth - i - 1) a m}
--
-- -- TODO ??? could remove duplecate cate by using bSplitAt
-- bDrop :: Int -> BruijnEnv a -> BruijnEnv a
-- bDrop n b = b {bruijnDepth = newDepth, bruijnMap = newM}
--     where (newM,_) = IM.split newDepth(bruijnMap b )
--           newDepth = bruijnDepth b - n
--
-- bExtend :: Int -> BruijnEnv a -> BruijnEnv a
-- bExtend n = bDrop (-n)
--
-- bFilter :: (a -> Bool) -> BruijnEnv a -> BruijnEnv a
-- bFilter f env = env {bruijnMap = IM.filter f $ bruijnMap env}
--
-- bSplitAt :: Int -> BruijnEnv  a -> (BruijnEnv a, [a])
-- bSplitAt n b = (b{bruijnDepth = newDepth,bruijnMap = low}, maybeToList pivot ++ map snd (IM.toAscList high))
--   where (low,pivot,high) = IM.splitLookup newDepth(bruijnMap b )
--         newDepth = bruijnDepth b - n
--
--
-- instance Functor BruijnEnv  where
--     fmap f b = b{bruijnMap = fmap f (bruijnMap b)}
--
-- mapWithBound :: (Bound -> a -> b) -> BruijnEnv a -> BruijnEnv b
-- mapWithBound f b@BruijnState{bruijnDepth = dept,bruijnMap =m} =
--     b {bruijnMap = IM.mapWithKey (\index a-> f (Bound $! dept -index -1)a  ) m}
--
