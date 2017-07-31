module ModifiedLambda
  ( module ModifiedLambda
  )
where

import LambdaF
import MTable
import qualified BruijnTerm as Lam
import BruijnTerm (BruijnTerm,incFree)
import ModificationTags
import qualified TaggedLambda  as Tag
import BruijnEnvironment

type LamTerm i = Tag.LamTerm i Bound (Modify i)
type Def i = Tag.Def i Bound (Modify i)

remember :: Modify () -> MTable -> MTable
remember modification s@MTable {getEnv = env} = remember' modification
  where
    remember' (Reorder n order)  = s {getEnv = bReorder env n order}
    remember' (Substitut n term) = s {getEnv = bInsertAt n (Subst (getDepth s) term) env}
    remember' (SubstitutT n term) = remember  (Substitut n $ proces s term) s

peek :: MTable -> LamTerm () -> (LamTermF () Bound (LamTerm ()), MTable)
peek modifications term = case term of
  Tag.Tag m t -> peek (remember m modifications ) t
  Tag.Val i v -> (ValF i v,modifications)
  Tag.Var _ b -> peekVar modifications b
  Tag.Appl t1 t2 -> (ApplF t1 t2,modifications)
  Tag.Lambda i n t -> (LambdaF i n t,insertT [Undefined depth] modifications)
  Tag.Let i defs t -> ( LetF i (map peekDef defs) t,insertT (map Undefined [depth .. depth + nDefs-1]) modifications )
    where
      nDefs = length defs
  where
    depth = getDepth modifications
    peekDef (Tag.Def i_ n_ t_) = DefF i_ n_ t_ --TODO can replaced if DefF is deafaul

peekVar :: MTable -> Bound -> (LamTermF () Bound (LamTerm ()),MTable)
peekVar modifications b@(Bound n) =
  let table = getEnv modifications
  in case bMaybeLookup b table of
    Just (Undefined depthDefined) -> (VarF () $ Bound $ depth- depthDefined - 1,modifications)
    Just (Subst depthDefined t2) ->
          -- TODO incfree can be fast with tag? /or modifyed mod
        peek empty $ Tag.tag $ incFree (depth - depthDefined) t2
      --TODO nsubst can be memorize wordt it ?
    Nothing -> ( VarF () (Bound $ n - nsubst table),modifications)
  where
    depth = getDepth modifications

applyModify :: LamTerm () -> BruijnTerm ()
applyModify term = proces empty term

proces ::  MTable  -> LamTerm () -> BruijnTerm ()
proces = unfold peek

procesDef :: MTable -> DefF () Bound (LamTerm ())  -> Lam.Def () Bound
procesDef modifications (DefF i n t) = Lam.Def i n (proces modifications t) -- TODO can replaced if DefF is deafaul

deepin :: LamTerm ()  -> LamTermF () Bound (LamTerm())
deepin (Tag.Var i n) = VarF i n
deepin (Tag.Appl t1 t2) = ApplF t1 t2
deepin (Tag.Val i v) = ValF i v
deepin (Tag.Lambda i n t) = LambdaF i n t
deepin (Tag.Tag m t) = deepinTags [m] t
deepin (Tag.Let i defs t) = LetF i (fmap deepinDef defs) t
  where
    deepinDef (Tag.Def i_ n_ t_) = DefF i_ n_ t_

deepinTags :: [Modify ()] -> LamTerm ()  -> LamTermF () Bound (LamTerm())
deepinTags tags (Tag.Tag m t) = deepinTags (m:tags) t
deepinTags tags (Tag.Var _ b) = fst $ peekVar(foldr remember empty tags) b
deepinTags _ (Tag.Val i v) = ValF i v
deepinTags tags t = let newTags = map (deepinTag (depthChange t)) tags
                in fmap (\subT -> foldr Tag.Tag subT newTags) (deepin t)

deepinTag :: Int -> Modify i -> Modify i
deepinTag n (Reorder d order) = Reorder (d+n) order
deepinTag _ _ = error "afsd"

depthChange :: Tag.LamTerm i n (Modify i)  -> Int
depthChange (Tag.Tag _ t) = depthChange t -- could be faster
depthChange Tag.Lambda {} = 1
depthChange (Tag.Let _ defs _) = length defs
depthChange _ = 0