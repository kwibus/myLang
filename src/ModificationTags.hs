-- | this Module defines tags for delaying modifications of BruijnTerm's
--
-- For some modification you have to rewrite to hole subtree. but if you tag ("TaggedLambda") that subtree with you modification, you can batch you modifications and do them only when needed.
module ModificationTags
( module ModificationTags
)
where

import LambdaF
import MTable
import BruijnTerm (BruijnTerm)
import qualified TaggedLambda  as Tag
import BruijnEnvironment

data Modify i = Reorder Int [Bound] -- ^ @[1, 0, 2]@ will make bound 0 -> 1, 1 -> 0 2 -> 2
              | SubstitutT Int (Tag.LamTerm () Bound (Modify())) -- ^ wil Substitut Bound 0 term. If higher index if deeper
              | Substitut Int (BruijnTerm i) -- ^ wil Substitut Bound 0 term. If higher index if deeper
              | IncFree Int Int
              deriving (Eq, Show)

--TODO doc tag scoping  / undefined var / binding
--  __    __
-- |  |  |  |
-- \.[0] \.10   -> \\.1:q
--
--    |____|

type LamTerm i = Tag.LamTerm i Bound (Modify i)

remember :: Modify () -> MTable -> MTable
remember modification s@MTable {getEnv = env} = remember' modification
  where
    remember' (Reorder n order)  = s {getEnv = bReorder env n order}
    remember' (Substitut n term) = substitute (Bound n) 0 term s
    remember' (SubstitutT n term) = remember  (Substitut n $ proces s term) s
    -- TODO implement ofset
    remember'  (IncFree 0 inc ) = incFree inc s

peek :: MTable -> LamTerm () -> (LamTermF () Bound (LamTerm ()), MTable)
peek modifications term = case term of
  Tag.Tag m t -> peek (remember m modifications ) t
  Tag.Val i v -> (ValF i v,modifications)
  Tag.Var _ b -> peekVar modifications b
  Tag.Appl t1 t2 -> (ApplF t1 t2,modifications)
  Tag.Lambda i n t -> (LambdaF i n t,insertT [Undefined depth] modifications)
  Tag.Let i defs t -> ( LetF i defs t,insertT (map Undefined [depth .. depth + nDefs-1]) modifications )
    where
      nDefs = length defs
  where
    depth = getDepth modifications

peekVar :: MTable -> Bound -> (LamTermF () Bound (LamTerm ()),MTable)
peekVar modifications b@(Bound n) =
  let table = getEnv modifications
  in case bMaybeLookup b table of
    Just (Undefined depthDefined) -> (VarF () $ Bound $ depth- depthDefined - 1,modifications)
    Just (Subst depthDefined t2) ->
        peek (incFree (depth - depthDefined)empty) $ Tag.tag t2
    Nothing -> ( VarF () (Bound $ n + incFreeFromStart modifications),modifications)
  where
    depth = getDepth modifications

applyModify :: LamTerm () -> BruijnTerm ()
applyModify term = proces empty term

proces ::  MTable  -> LamTerm () -> BruijnTerm ()
proces = unfold peek

deepin :: LamTerm ()  -> LamTermF () Bound (LamTerm())
deepin (Tag.Var i n) = VarF i n
deepin (Tag.Appl t1 t2) = ApplF t1 t2
deepin (Tag.Val i v) = ValF i v
deepin (Tag.Lambda i n t) = LambdaF i n t
deepin (Tag.Tag m t) = deepinTags [m] t
deepin (Tag.Let i defs t) = LetF i defs t

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
