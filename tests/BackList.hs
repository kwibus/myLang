module BackList where

import Control.Monad
import Control.Applicative

data BackList a = Steps !Int | List [a] deriving Show

instance Functor BackList where
  fmap = liftM

instance Applicative BackList where
  pure = return
  (<*>) = ap

instance Monad BackList where
    return a = List [a]
    Steps i >>= _ = Steps i
    List [] >>= _ = error "emty backlist"
    List [l] >>= f = f l
    List l >>= f = case msum (map f l) of
          Steps i -> Steps $! i + 1
          List l2 -> List l2

instance MonadPlus BackList where
    mzero = Steps 1
    mplus (Steps i ) (Steps j ) = Steps $! max i j
    mplus (Steps {}) b = b
    mplus (List l ) b = List $ l ++ toList b

instance Alternative BackList where
   empty = mzero
   (<|>) = mplus

try :: [a] -> BackList a
try [] = Steps 1
try as = List as

tryM :: [BackList a] -> BackList a
tryM [] = Steps 1
tryM list = case msum list of
   List [] -> error "null list" --  Steps 1
   List l -> List l
   Steps i -> Steps $! i + 1

toList :: BackList a -> [a]
toList (Steps {}) = []
toList (List l) = l

backsteps :: BackList a -> Int
backsteps List {} = 0
backsteps (Steps i) = i
