module BruijnEnvironment where

import qualified Data.IntMap as IM

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
data BruiEnv a = BruiState
     { bruijnDepth :: Int
     , bruijnMap :: IM.IntMap a
     } deriving (Show, Eq)

-- TODO remove
toInt :: Bound -> Int
toInt (Bound i) = i

bEmtyEnv :: BruiEnv a
bEmtyEnv = BruiState
    { bruijnDepth = 0
    , bruijnMap = IM.empty
    }
bMember :: Bound -> BruiEnv a -> Bool
bMember (Bound i) BruiState {bruijnDepth = depth, bruijnMap = m}
    = IM.member (depth - i - 1) m

bLookup :: Bound -> BruiEnv a -> a
bLookup (Bound i) BruiState {bruijnDepth = depth, bruijnMap = m} =
    m IM.! (depth - i - 1)

bInsert :: a -> BruiEnv a -> BruiEnv a
bInsert a b@BruiState {bruijnDepth = depth, bruijnMap = m} =
     b {bruijnDepth = depth + 1, bruijnMap = IM.insert depth a m }

bToList :: BruiEnv a -> [(Int, a)]
bToList BruiState {bruijnMap = m} = IM.toList m
