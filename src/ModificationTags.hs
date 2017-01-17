-- | this Module defines tags for delaying modifications of BruijnTerm's
--
-- For some modification you have to rewrite to hole subtree. but if you tag ("TaggedLambda") that subtree with you modification, you can batch you modifications and do them only when needed.
module ModificationTags where

import BruijnEnvironment
import BruijnTerm

data Modify i = Reorder [Bound] -- ^ @[1, 0, 2]@ will make bound 0 -> 1, 1 -> 0 2 -> 2
              | Substitut (BruijnTerm i) -- ^ wil Substitut Bound 0 term. If higher index if deeper
              deriving (Eq, Show)
