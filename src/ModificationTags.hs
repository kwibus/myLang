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

-- TODO why tag with i
data Modify i = Reorder Int [Bound] -- ^ @[1, 0, 2]@ will make bound 0 -> 1, 1 -> 0 2 -> 2
              | SubstitutT Int (Tag.LamTerm () () Bound (Modify())) -- ^ wil Substitut Bound 0 term. If higher index if deeper
              | Substitut Int (BruijnTerm i ()) -- ^ wil Substitut Bound 0 term. If higher index if deeper
              | IncFree Int Int
              deriving (Eq, Show)

--TODO doc tag scoping  / undefined var / binding
--  __    __
-- |  |  |  |
-- \.[0] \.10   -> \\.10
--
--    |____|

type LamTerm i j = Tag.LamTerm i j Bound (Modify i)

remember :: Modify () -> MTable -> MTable
remember (Reorder n order) mtable = reorder n order mtable
remember (Substitut n term) mtable = substitute (Bound n) 0 term mtable
remember (SubstitutT n term) mtable = remember  (Substitut n $ proces mtable term) mtable
-- TODO implement ofset
remember  (IncFree 0 inc ) mtable = incFree inc mtable

-- | peek will apply modifications stored in mtabel and apply it on first node in ast
-- and will give back a new 'MTable' that can be used to view the subtrees
--
-- you should only use the new 'MTable' on the found subtrees because:
--
-- * 'MTable' keep track of depth of the subtree,
--
-- * if a variable is 'substituted' for new subtree, then that subtree need to be ajusted
--   mtable keep track of this.
--
-- * if peek encounters a 'Tag' that specify a modification that should be done on its subtree
--   that is stored in mtable
-- but this is not enforced, if you want a save variant use "Unprocessed"
peek :: MTable -> LamTerm () ()-> (LamTermF () () Bound (LamTerm () ()), MTable)
peek modifications term = case term of
  Tag.Tag m t -> peek (remember m modifications ) t
  Tag.Val i v -> (ValF i v,modifications)
  Tag.Var _ b -> case peekVar modifications b of
    (Left newB) -> (VarF () newB,modifications)
    (Right (t,newM)) -> peek newM $ Tag.tag t
  Tag.Appl t1 t2 -> (ApplF t1 t2,modifications)
  Tag.Lambda i n t -> (LambdaF i n t,extraSparceInsertUndefind 1  modifications)
  Tag.Let i defs t -> ( LetF i defs t,extraSparceInsertUndefind (length defs) modifications )

applyModify :: LamTerm () () -> BruijnTerm () ()
applyModify term = proces empty term

proces ::  MTable  -> LamTerm () () -> BruijnTerm () ()
proces = unfold peek

deepin :: LamTerm () () -> LamTermF () () Bound (LamTerm() () )
deepin (Tag.Var i n) = VarF i n
deepin (Tag.Appl t1 t2) = ApplF t1 t2
deepin (Tag.Val i v) = ValF i v
deepin (Tag.Lambda i n t) = LambdaF i n t
deepin (Tag.Tag m t) = deepinTags [m] t
deepin (Tag.Let i defs t) = LetF i defs t

deepinTags :: [Modify ()] -> LamTerm () () -> LamTermF () () Bound (LamTerm () ())
deepinTags tags (Tag.Tag m t) = deepinTags (m:tags) t
deepinTags tags (Tag.Var _ b) = case peekVar (foldr remember empty tags) b of
  (Left newB) -> VarF () newB
  (Right (t,_)) -> deepinTags tags $ Tag.tag t

deepinTags _ (Tag.Val i v) = ValF i v
deepinTags tags t = fmap addNewTags (deepin t)
  where
    newTags = map (deepinTag (depthChange t)) tags
    addNewTags subT = foldr Tag.Tag subT newTags

deepinTag :: Int -> Modify i -> Modify i
deepinTag n (Reorder d order) = Reorder (d+n) order
deepinTag _ _ = error "afsd"

depthChange :: Tag.LamTerm i j n (Modify i)  -> Int
depthChange (Tag.Tag _ t) = depthChange t -- could be faster
depthChange Tag.Lambda {} = 1
depthChange (Tag.Let _ defs _) = length defs
depthChange _ = 0
