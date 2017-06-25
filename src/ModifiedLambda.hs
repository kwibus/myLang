{-# LANGUAGE LambdaCase #-}
module ModifiedLambda
  ( module ModifiedLambda
  )
where

import LambdaF
import BruijnEnvironment

import qualified BruijnTerm as Lam
import BruijnTerm (BruijnTerm,incFree)

import qualified TaggedLambda  as Tag

import ModificationTags
type LamTerm i = Tag.LamTerm i Bound (Modify i)
type Def i = Tag.Def i Bound (Modify i)

-- inc :: Int -> MTable -> MTable
-- inc n t@MTable{} = t{getDepth = getDepth t + n}

-- TODO maybe make a mtable separte module rename
remember :: Modify () -> MTable -> MTable
remember modification s@MTable {getEnv = env} = remember' modification
  where
    remember' (Reorder n order)  = s {getEnv = bReorder env n order}
    remember' (SubstitutT n term) = remember  (Substitut n $ proces s term) s
    remember' (Substitut n term) = s {getEnv = bInsertAt n (Subst (getDepth s) term) env}

-- TODO env rename name , get simbolTabe
data MTable = MTable
            { getDepth :: Int
            , getEnv :: BruijnEnv (Symbol ())}
              deriving (Eq, Show)

drop :: Int -> MTable -> MTable
drop n m = m{getDepth = getDepth m - n
          ,getEnv = bDrop n $ getEnv m}

empty :: MTable
empty = MTable 0 bEmtyEnv

data Symbol i = Subst Int (BruijnTerm i)
           | Undefined Int
           deriving (Show, Eq)

--TODO remove
nsubst :: BruijnEnv (Symbol i) -> Int
nsubst = bSize . bFilter (\ case
                 Subst {} -> True
                 _ -> False)

-- TODO rename mod move to differ
peek :: MTable -> LamTerm () -> (LamTermF () Bound (LamTerm ()), MTable)
peek mod term = case term of
  Tag.Tag m t -> peek (remember m mod ) t
  Tag.Val i v -> (ValF i v,mod)
  Tag.Var i b -> peekVar mod b
  Tag.Appl t1 t2 -> (ApplF t1 t2,mod)
  Tag.Lambda i n t -> (LambdaF i n t,insertT [Undefined $ getDepth mod] mod)
  Tag.Let i defs t -> ( LetF i (map peekDef defs) t,insertT (map Undefined [depth .. depth + nDefs-1]) mod )
        where
          nDefs = length defs
          depth = getDepth mod
          peekDef (Tag.Def i_ n_ t_) = DefF i_ n_ t_ --TODO relace

peekVar :: MTable -> Bound -> (LamTermF () Bound (LamTerm ()),MTable)
peekVar mod b@(Bound n) =
    let table = getEnv mod
    in case bMaybeLookup b table of
          Just (Undefined depthDefined) -> (VarF () $ Bound $ getDepth mod - depthDefined - 1,mod)
          Just (Subst orignalsize t2) ->
            -- incfree can be a tag
            -- is it necesary to peak twice
              peek empty $ Tag.tag $ incFree (getDepth mod - orignalsize) t2
          Nothing -> ( VarF () (Bound $ n - nsubst table),mod) --TODO nsubst can be memorize wordt it ?

-- rename
insertT :: [Symbol ()] -> MTable -> MTable
insertT refs s@MTable {getEnv = env, getDepth = depth}
    = s {getEnv = bInserts refs env
        , getDepth = depth + length refs}

applyModify :: LamTerm () -> BruijnTerm ()
applyModify term =  proces empty term

--TODO can be short circuit
proces ::  MTable  -> LamTerm () -> BruijnTerm ()
proces = unfold peek

procesDef :: MTable -> DefF () Bound (LamTerm ())  -> Lam.Def () Bound
procesDef modifications (DefF i n t) = Lam.Def i n (proces modifications t)

deepin :: LamTerm ()  -> LamTermF () Bound (LamTerm())
deepin (Tag.Var i n) = VarF i n
deepin (Tag.Appl t1 t2) = ApplF t1 t2
deepin (Tag.Val i v) = ValF i v
deepin (Tag.Lambda i n t) = LambdaF i n t
deepin (Tag.Let i defs t) = LetF i (fmap deepinDef defs) t
  where
    deepinDef (Tag.Def i_ n_ t_) = DefF i_ n_ t_
deepin (Tag.Tag m t) = go [m] t
  where
    go :: [Modify ()] -> LamTerm ()  -> LamTermF () Bound (LamTerm())
    go tags (Tag.Tag m t) = go (m:tags) t
    go tags (Tag.Var _ b) = fst $ peekVar(foldr remember empty tags) b
    go _ (Tag.Val i v) = ValF i v
    go tags t = let newTags = map (deepinTag (depthChange t)) tags
                in fmap (\subT -> foldr Tag.Tag subT newTags) (deepin t)

    deepinTag :: Int -> Modify i -> Modify i
    deepinTag n (Reorder d order) = Reorder (d+n) order
    deepinTag _ _ = error "afsd"

    depthChange :: Tag.LamTerm i n (Modify i)  -> Int
    depthChange (Tag.Tag _ t) = depthChange t -- could be faster
    depthChange Tag.Lambda {} = 1
    depthChange (Tag.Let _ defs _) = length defs
    depthChange _ = 0
