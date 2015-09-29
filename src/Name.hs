module Name where

import Control.Monad

-- it is a newtype not Type String so it can be a instant of typeclass
-- needed for test/ArbiRef

newtype Name = Name String deriving (Eq, Show, Ord)

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']
