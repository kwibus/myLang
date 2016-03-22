module BackList where

import Control.Monad
import Control.Applicative

data BackList a = Failures !Int | List [a] deriving Show

instance Functor BackList where
  fmap = liftM

instance Applicative BackList where
  pure = return
  (<*>) = ap

instance Monad BackList where
    return a = List [a]
    Failures i >>= _ = Failures i
    -- List [] >>= _ = error "emty backlist"
    List l >>= f = tryM (map f l)

--  TODO remove is not mplus
instance MonadPlus BackList where
    mzero = Failures 1
    mplus (Failures i ) (Failures j ) = Failures $! i + j
    mplus Failures {} b = b
    mplus (List l ) b = List $ l ++ toList b

instance Alternative BackList where
   empty = mzero
   (<|>) = mplus

try :: [a] -> BackList a
try [] = Failures 1
try as = List as

tryM :: [BackList a] -> BackList a
tryM [] = Failures 1
tryM list = foldr1 mplus list

toList :: BackList a -> [a]
toList Failures {} = []
toList (List l) = l

failures :: BackList a -> Int
failures List {} = 0
failures (Failures i) = i
