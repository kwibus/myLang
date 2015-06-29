module Names where

import Control.Monad
newtype Name = Name String deriving (Eq,Show,Ord)

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']
