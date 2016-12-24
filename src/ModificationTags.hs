{-# LANGUAGE LambdaCase #-}
module ModificationTags
    ( Modify(..)
    , Ref  (..)
    , proces
    , rember
    )
where

import BruijnEnvironment
import qualified TaggedLambda as Tag
import qualified Lambda as Lam
import BruijnTerm
import Name
import Value

-- | [1 , 0 ,2] will make bound 0 -> 1, 1->0 2->2

data Modify i = Reorder [Bound]
              | Substitut (Lam.LamTerm i Bound)
              deriving (Eq, Show)

data LamTerm i n a = Lambda i Name a
            | Appl a a
            | Var i n
            | Val i Value
            | Let i [Def i n a] a
            deriving (Eq, Show)

data Def i n a = Def i Name a deriving (Eq, Show)

data Ref i = Subst (Lam.LamTerm i Bound)  | Keep Int deriving (Show, Eq)

rember :: Modify i -> BruijnEnv (Ref i) -> BruijnEnv (Ref i)
rember (Reorder order) env = bReorder env order
rember (Substitut term) env = bInsert (Subst term) env

nsubst::  BruijnEnv (Ref i) -> Int
nsubst = bSize . bFilter (\case
                 Subst {}-> True
                 _ -> False  )

--TODO add comments
proces :: Tag.LamTerm i Bound (Modify i)-> Lam.LamTerm i Bound
proces term = go term 0 bEmtyEnv
  where
    go :: Tag.LamTerm i Bound (Modify i)-> Int -> BruijnEnv (Ref i)-> Lam.LamTerm i Bound
    go (Tag.Tag m t) depth env= go t depth $ rember m env
    go (Tag.Var i b@(Bound n)) depth env = case bMaybeLookup b env of
        Just (Keep depthDefined) -> Lam.Var i $ Bound $ depth - depthDefined -1
        Just (Subst t2) -> incFree (depth - nsubst ( bDrop (n+1) env)) t2
        Nothing -> Lam.Var i (Bound $ n - nsubst env)
    go (Tag.Val i v) _ _ = Lam.Val i v
    go (Tag.Lambda i n t) depth env = Lam.Lambda i n $ go t (depth + 1)(bInsert (Keep depth) env)
    go (Tag.Appl t1 t2) depth env = Lam.Appl (go t1 depth env) (go t2 depth env)
    go (Tag.Let i defs t) depth env = Lam.Let i (map godefs defs) $ go t newDepth newEnv
      where
        godefs (Tag.Def i_ n_ t_) = Lam.Def i_ n_ $ go t_ newDepth newEnv
        newDepth = depth + length  defs
        newEnv = bInserts (map Keep [depth..newDepth -1]) env

-- TODO remove duplcated incfree (also in eval)
incFree :: Int -> BruijnTerm i -> BruijnTerm i
incFree 0 term = term
incFree increase  term = go 0 term
  where
    go :: Int -> BruijnTerm i -> BruijnTerm i
    go depth (Lam.Lambda i n t) =Lam.Lambda i n $ go (depth+1) t
    go depth (Lam.Appl t1 t2) = Lam.Appl (go depth t1)(go depth t2)
    go depth (Lam.Var i (Bound n)) | n >= depth  = Lam.Var i $ Bound $ n+increase
                               | otherwise = Lam.Var i (Bound n)
    go depth (Lam.Let i defs t) = Lam.Let i (fmap incDefs defs) $ go newDepth t
      where
        newDepth = depth +length defs
        incDefs (Lam.Def is ns ts) = Lam.Def is ns $ go newDepth ts
    go _ (Lam.Val i v) = Lam.Val i v
