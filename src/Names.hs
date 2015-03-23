module Names where

import Control.Monad
type Name = String

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']
