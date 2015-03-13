module BruijnTerm where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Exception.Base

import Enviroment
import Lambda
import Type

type Index = Int

type BruijnTerm v = LamTerm v Bound

toList :: BruiEnv a -> [(Int, a)]
toList BruiState {bruiMap = m} = IM.toList m

lam2Bruijn :: LamTerm v Name -> BruijnTerm v
lam2Bruijn t = go t 0 M.empty
  where go (Var n) depth env = Var $ Bound (depth - (env M.! n) - 1)
        go (Val v) _ _ = Val v
        go (Lambda n t1) depth env = Lambda n $
                     go t1 (depth + 1) (M.insert n depth env)
        go (Appl t1 t2) depth env = Appl (go t1 depth env) (go t2 depth env)

newFreeVar :: FreeEnv (Type Free) -> (FreeEnv (Type Free), Free)
newFreeVar b@FreeState {freeVars = n } =
   let newVar = (n + 1)
   in (b {freeVars = n + 1}, Free newVar)

bSupsitute :: Free -> a -> BruiEnv a -> BruiEnv a
bSupsitute (Free i) a env@BruiState {bruiMap = m} =
    assert (IM.member (-i) m ) $ env {bruiMap = IM.insert (-i) a m}

bruijn2Lam :: BruijnTerm v -> LamTerm v Name
bruijn2Lam t = go t bEmtyEnv
  where go :: BruijnTerm v -> BruiEnv Name -> (LamTerm v Name)
        go (Var i) env = Var $ if bMember i env
                                    then bLookup i env
                                    else error $ "varibale " ++
                                           show i ++ "is used before defined"
        go (Val v) _ = Val v
        go (Appl e1 e2 ) env = Appl (go e1 env ) (go e2 env )
        go (Lambda n e1) env = let (nenv, _) = bInsert n env
                               in Lambda n (go e1 nenv)
