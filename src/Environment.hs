module Environment where

import qualified Data.IntMap as IM
import Control.Exception.Base
import Data.Coerce

--TODO split op free env and bound env

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

-- | Free is also a wrapper arround Int but is used when BruijnIndex can`t be used
-- it uses a absolute inex
-- and bacause Bound And Free are implemented with newtype you can't execdentalyy mix them
newtype Free = Free Int deriving (Eq, Show, Ord)

-- | conversion bound to Free. This can be unsafe. because it keeps the same Int representation,
-- and two Bound that refer to the same thing may have different Int index
bound2Free :: Bound -> Free
bound2Free = coerce

-- TODO remve clase
class ToInt a where
    toInt :: a -> Int

instance ToInt Bound where
    toInt = coerce

instance ToInt Free where
    toInt = coerce

--TODO replace with list
--TODO Fix name to BruijnEnv
data BruiEnv a = BruiState
     { bruiDepth :: Int
     , bruiMap :: IM.IntMap a
     } deriving (Show, Eq)

type FreeEnv a = IM.IntMap a

bEmtyEnv :: BruiEnv a
bEmtyEnv = BruiState
    { bruiDepth = 0
    , bruiMap = IM.empty
    }

fEmtyEnv :: FreeEnv a
fEmtyEnv = IM.empty

bToList :: BruiEnv a -> [(Int, a)]
bToList BruiState {bruiMap = m} = IM.toList m

fFromList :: [(a, Free)] -> FreeEnv a
fFromList list = IM.fromList $ map convert list
    where convert (a, Free i) = (i, a)

bInsert :: a -> BruiEnv a -> BruiEnv a
bInsert a b@BruiState {bruiDepth = depth, bruiMap = m} =
     b {bruiDepth = depth + 1, bruiMap = IM.insert depth a m }

--  TODO inconsistend name
finsertAt :: a -> Free -> FreeEnv a -> FreeEnv a
finsertAt a (Free i) = IM.insert i a

bLookup :: Bound -> BruiEnv a -> a
bLookup (Bound i) BruiState {bruiDepth = depth, bruiMap = m} =
    m IM.! (depth - i - 1)

fLookup :: Free -> FreeEnv a -> a
fLookup (Free i) m =
    m IM.! i

bMember :: Bound -> BruiEnv a -> Bool
bMember (Bound i) BruiState {bruiDepth = depth, bruiMap = m}
    = IM.member (depth - i - 1) m

fMember :: Free -> FreeEnv a -> Bool
fMember (Free i) = IM.member i
