module BruijnTerm where

import qualified Data.IntMap as IM
import qualified Data.Map as M

import Vallue
import Lambda

type Index = Int
data BruijnTerm = BLambda Name BruijnTerm
                | BAppl BruijnTerm BruijnTerm
                | Bound Index
                | BVal Vallue deriving (Eq, Show)
             -- | Freevar  should not exsist

-- data Bvariable = Bound Index | BVal Vallue deriving (Show, Eq)

lam2Bruijn :: LamTerm -> BruijnTerm
lam2Bruijn t = go t 0 M.empty
  where go (Var n) depth env = Bound (depth - (env M.! n) - 1)
        go (Val v) _ _ = BVal v
        go (Lambda n t1) depth env = BLambda n $ go t1 (depth + 1) (M.insert n depth env)
        go (Appl t1 t2) depth env = BAppl (go t1 depth env) (go t2 depth env)

bruijn2Lam :: BruijnTerm -> LamTerm
bruijn2Lam t = go t 0 IM.empty
  where go (Bound i) depth env = Var $ env IM.! (depth - i - 1 )
        go (BVal v) _ _ = Val v
        go (BAppl t1 t2 ) depth env = Appl (go t1 depth env) (go t2 depth env)
        go (BLambda n t1) depth env = Lambda n $ go t1 (depth + 1) (IM.insert depth n env)
