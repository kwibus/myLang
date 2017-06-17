-- | this Module defines tags for delaying modifications of BruijnTerm's
--
-- For some modification you have to rewrite to hole subtree. but if you tag ("TaggedLambda") that subtree with you modification, you can batch you modifications and do them only when needed.
module ModificationTags where

import BruijnEnvironment
import BruijnTerm
import TaggedLambda as Tag
data Modify i = Reorder Int [Bound] -- ^ @[1, 0, 2]@ will make bound 0 -> 1, 1 -> 0 2 -> 2
              | SubstitutT Int (Tag.LamTerm () Bound (Modify())) -- ^ wil Substitut Bound 0 term. If higher index if deeper
              | Substitut Int (BruijnTerm i) -- ^ wil Substitut Bound 0 term. If higher index if deeper
              deriving (Eq, Show)

--TODO doc tag scoping  / undefined var / binding
--  __    __
-- |  |  |  |
-- \.[0] \.10   -> \\.1:q
--
--    |____|
