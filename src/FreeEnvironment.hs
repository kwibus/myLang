module FreeEnvironment where

import qualified Data.IntMap as IM

-- | Free is also a wrapper arround Int but is used when BruijnIndex can`t be used
-- it uses a absolute inex
-- and bacause Bound And Free are implemented with newtype you can't execdentalyy mix them
newtype Free = Free Int deriving (Eq, Show, Ord)

type FreeEnv a = IM.IntMap a

fEmtyEnv :: FreeEnv a
fEmtyEnv = IM.empty

fFromList :: [(a, Free)] -> FreeEnv a
fFromList list = IM.fromList $ map convert list
  where convert (a, Free i) = (i, a)

finsertAt :: a -> Free -> FreeEnv a -> FreeEnv a
finsertAt a (Free i) = IM.insert i a

fMaybeLookup ::Free -> FreeEnv a -> Maybe a
fMaybeLookup (Free i) m = IM.lookup i m

fLookup :: Free -> FreeEnv a -> a
fLookup (Free i) m = m IM.! i

fMember :: Free -> FreeEnv a -> Bool
fMember (Free i) = IM.member i
