module ModificationTags where

import BruijnEnvironment
import qualified TaggedLambda as Tag
import Lambda
-- | [1 , 0 ,2] will make bound 0 -> 1, 1->0 2->2
data Modify = Reorder [Bound]
              deriving (Eq, Show)

rember :: Modify -> BruijnEnv a -> BruijnEnv a
rember (Reorder order) env = bReorder env order

proces :: Tag.LamTerm i Bound Modify -> LamTerm i Bound
proces term = go term 0 bEmtyEnv
  where
    go :: Tag.LamTerm i Bound Modify -> Int -> BruijnEnv Int -> LamTerm i Bound
    go (Tag.Tag m t) depth env= go t depth $ rember m env
    go (Tag.Var i b) depth env = case bMaybeLookup b env of
        Just n -> Var i $ Bound $ depth - n -1
        Nothing -> Var i b
    go (Tag.Val i v) _ _ = Val i v
    go (Tag.Lambda i n t) depth env = Lambda i n $ go t (depth + 1)(bInsert depth env)
    go (Tag.Appl t1 t2) depth env = Appl (go t1 depth env) (go t2 depth env)
    go (Tag.Let i defs t) depth env = Let i (map godefs defs) $ go t newDepth newEnv
      where
        godefs (Tag.Def i_ n_ t_) = Def i_ n_ $ go t_ newDepth newEnv
        newDepth = depth + length  defs
        newEnv = bInserts [depth..newDepth -1] env
