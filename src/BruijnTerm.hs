module BruijnTerm where

import qualified Data.IntMap as IM
import qualified Data.Map as M

import Lambda

type Index = Int

type BruijnTerm v = LamTerm v Int

lam2Bruijn :: LamTerm v Name -> BruijnTerm v
lam2Bruijn t = go t 0 M.empty
  where go (Var n) depth env = Var (depth - (env M.! n) - 1)
        go (Val v) _ _ = Val v
        go (Lambda n t1) depth env = Lambda n $ go t1 (depth + 1) (M.insert n depth env)
        go (Appl t1 t2) depth env = Appl (go t1 depth env) (go t2 depth env)

bruijn2Lam :: BruijnTerm v -> LamTerm v Name
bruijn2Lam t = go t 0 IM.empty
  where go (Var i) depth env = Var $ env IM.! (depth - i - 1 )
        go (Val v) _ _ = Val v
        go (Appl t1 t2 ) depth env = Appl (go t1 depth env) (go t2 depth env)
        go (Lambda n t1) depth env = Lambda n $ go t1 (depth + 1) (IM.insert depth n env)

transform :: Monad m => (LamTerm v1 i1 -> m (LamTerm v2 i2))
                       -> LamTerm v1 i1
                       -> m (LamTerm v2 i2)
transform f (Appl e1 e2) =
  do e1' <- transform f e1
     e2' <- transform f e2
     return $ Appl e1' e2'
transform f (Lambda n e) =
  do e' <- transform f e
     return $ Lambda n e'
transform f t = f t
