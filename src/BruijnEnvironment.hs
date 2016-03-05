module BruijnEnvironment where

import qualified Data.IntMap as IM
import Control.Exception.Base
import Data.Maybe
--
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
data BruijnEnv a = BruijnState
     { bruijnDepth :: Int
     , bruijnMap :: IM.IntMap a
     } deriving (Show, Eq)

toInt :: Bound -> Int
toInt (Bound i) = i

bEmtyEnv :: BruijnEnv a
bEmtyEnv = BruijnState
    { bruijnDepth = 0
    , bruijnMap = IM.empty
    }
bMember :: Bound -> BruijnEnv a -> Bool
bMember b e= isJust $ bMaybeLookup b e

bLookup :: Bound -> BruijnEnv a -> a
bLookup (Bound i) BruijnState {bruijnDepth = depth, bruijnMap = m} =
    m IM.! (depth - i - 1)

bMaybeLookup :: Bound -> BruijnEnv a -> Maybe a
bMaybeLookup (Bound i) BruijnState {bruijnDepth = depth, bruijnMap = m} =
    IM.lookup (depth - i - 1) m

bInsert :: a -> BruijnEnv a -> BruijnEnv a
bInsert a b@BruijnState {bruijnDepth = depth, bruijnMap = m} =
     b {bruijnDepth = depth + 1, bruijnMap = IM.insert depth a m }

bToList :: BruijnEnv a -> [(Int, a)]
bToList BruijnState {bruijnMap = m} = IM.toList m

bReplace :: Bound -> a -> BruijnEnv a -> BruijnEnv a
bReplace (Bound i) a b@BruijnState {bruijnDepth = depth, bruijnMap = m} =
  assert (bMember (Bound i) b)
  b {bruijnMap = IM.insert (depth - i - 1) a m}
