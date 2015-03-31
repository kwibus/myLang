{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Enviroment where

import qualified Data.IntMap as IM
import Control.Exception.Base
import Data.Foldable

newtype Bound = Bound Int deriving (Eq, Show)
newtype Free = Free Int deriving (Eq,Show)


data BruiEnv a = BruiState
     { bruiDepth :: Int
     , bruiMap :: IM.IntMap a
     } deriving (Show, Eq, Foldable, Functor)

type FreeEnv a = IM.IntMap a

bEmtyEnv :: BruiEnv a
bEmtyEnv = BruiState
    { bruiDepth = 0
    , bruiMap = IM.empty
    }

fEmtyEnv :: FreeEnv a
fEmtyEnv = IM.empty

bInsert :: a -> BruiEnv a -> (BruiEnv a, Bound)
bInsert a b@BruiState {bruiDepth = depth, bruiMap = m} =
 (b {bruiDepth = depth + 1, bruiMap = IM.insert depth a m }, Bound depth)

binsertAt :: a -> Bound -> BruiEnv a -> BruiEnv a
binsertAt a (Bound i ) e@BruiState {bruiMap = m, bruiDepth = dept} =
    let newDept = if (i > dept) then i else dept
    in e {bruiMap = IM.insert newDept a m, bruiDepth = newDept + 1 }

finsertAt :: a -> Free -> FreeEnv a -> FreeEnv a
finsertAt a (Free i) m = IM.insert i a m

resetDepth :: BruiEnv a -> BruiEnv a -> BruiEnv a
resetDepth BruiState {bruiDepth = dept} b = b {bruiDepth = dept}

bLookup :: Bound -> BruiEnv a -> a
bLookup (Bound i) BruiState {bruiDepth = depth, bruiMap = m} =
    assert (IM.member (depth - i - 1 ) m)
    assert (i >= 0)
    m IM.! (depth - i - 1)

fLookup :: Free -> FreeEnv a -> a
fLookup (Free i) m =
    assert (IM.member i m)
    assert (i >= 0)
    m IM.! i

bMember :: Bound -> BruiEnv a -> Bool
bMember (Bound i) BruiState {bruiDepth = depth, bruiMap = m}
    = IM.member (depth - i - 1) m

fMember :: Free -> FreeEnv a -> Bool
fMember (Free i) m = IM.member i m
