{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Enviroment where

import qualified Data.IntMap as IM
import Control.Exception.Base
import Data.Foldable

newtype Bound = Bound Int deriving (Eq, Show)
newtype Free = Free Int deriving (Eq, Show)


data BruiEnv a = BruiState
     { bruiDepth :: Int
     , bruiMap :: IM.IntMap a
     } deriving (Show, Eq, Foldable, Functor)

data FreeEnv a = FreeState
     { freeVars :: Int
     , freeMap :: IM.IntMap a
     } deriving (Show, Eq, Foldable, Functor)

bEmtyEnv :: BruiEnv a
bEmtyEnv = BruiState
    { bruiDepth = 0
    , bruiMap = IM.empty
    }

fEmtyEnv :: FreeEnv a
fEmtyEnv = FreeState
    { freeVars = 0
    , freeMap = IM.empty
    }

bInsert :: a -> BruiEnv a -> (BruiEnv a, Bound)
bInsert a b@BruiState {bruiDepth = depth, bruiMap = m} =
 (b {bruiDepth = depth + 1, bruiMap = IM.insert depth a m }, Bound depth)

binsertAt :: a -> Bound -> BruiEnv a -> BruiEnv a
binsertAt a (Bound i ) e@BruiState {bruiMap = m, bruiDepth = dept} =
    let newDept = if (i > dept) then i else dept
    in e {bruiMap = IM.insert newDept a m, bruiDepth = newDept + 1 }

finsertAt :: a -> Free -> FreeEnv a -> FreeEnv a
finsertAt a (Free i ) e@FreeState {freeMap = m, freeVars = n } =
    let newFreeVars = if i > n then i else n
    in e {freeMap = IM.insert (i) a m, freeVars = newFreeVars}

resetDepth :: BruiEnv a -> BruiEnv a -> BruiEnv a
resetDepth BruiState {bruiDepth = dept} b = b {bruiDepth = dept}

bLookup :: Bound -> BruiEnv a -> a
bLookup (Bound i) BruiState {bruiDepth = depth, bruiMap = m} =
    assert (IM.member (depth - i - 1 ) m)
    assert (i >= 0)
    m IM.! (depth - i - 1)

fLookup :: Free -> FreeEnv a -> a
fLookup (Free i) FreeState {freeMap = m} =
    assert (IM.member i m)
    assert (i >= 0)
    m IM.! i

bMember :: Bound -> BruiEnv a -> Bool
bMember (Bound i) BruiState {bruiDepth = depth, bruiMap = m}
    = IM.member (depth - i - 1) m

fMember :: Free -> FreeEnv a -> Bool
fMember (Free i) FreeState {freeMap = m} = IM.member i m
