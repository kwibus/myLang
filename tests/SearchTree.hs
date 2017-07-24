{-# LANGUAGE UndecidableInstances, FlexibleContexts, MultiWayIf #-}
module SearchTree where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Maybe

data Tree a = Node [Tree a] | Leaf a deriving (Show, Eq)

instance Functor Tree where
  fmap = liftM

instance Applicative Tree where
  pure = return
  (<*>) = ap

instance Monad Tree where
    return = Leaf
    Leaf a >>= f = f a
    Node ls >>= f = Node $ map (>>=f) ls

toList :: Tree a -> [a]
toList (Leaf a) = [a]
toList (Node ls) = concatMap toList ls

getbranches :: Tree a -> [Tree a]
getbranches (Leaf a) = [Leaf a]
getbranches (Node as) = as

instance Alternative Tree where
   empty = mzero
   (<|>) = mplus

instance MonadPlus Tree where
    mzero = Node []
    mplus a@(Leaf _) b = Node $ a : getbranches b
    mplus (Node as) b = Node $ as ++ getbranches b

newtype TreeT m a = TreeT {run :: m (Tree a)} --TODO renam run

instance Monad m => Functor (TreeT m) where
  fmap = liftM

instance Monad m => Applicative (TreeT m ) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (TreeT m) where
    return = TreeT . return . return
    m >>= f = TreeT $ run m >>= \ a ->
            case a of
                Leaf l -> run (f l)
                Node ls -> Node <$> mapM (run . (>>= f) . TreeT . return) ls

instance MonadTrans TreeT where
    lift = TreeT . fmap return

instance Monad m => Alternative (TreeT m) where
   empty = mzero
   (<|>) = mplus

instance Monad m => MonadPlus (TreeT m ) where
    mzero = TreeT $ return $ Node []
    mplus ma mb = TreeT $ liftM2 mplus (run ma) (run mb)

toListT :: Functor m => TreeT m a -> m [a]
toListT tree = toList <$> run tree

newtype SearchTree m a = Search {search :: TreeT m (Maybe a)}

instance Eq (m (Tree (Maybe a))) => Eq (SearchTree m a) where
    (Search (TreeT a)) == (Search (TreeT b)) = a == b

instance Show (m (Tree (Maybe a))) => Show (SearchTree m a) where
    show (Search (TreeT a)) = show a

instance Monad m => Functor (SearchTree m) where
  fmap = liftM

instance Monad m => Applicative (SearchTree m ) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (SearchTree m) where
    return = Search . return . Just
    m >>= f = Search $ TreeT $ (run . search) m >>= \ a ->
            case a of
                Leaf l -> case l of
                    Nothing -> return $ Leaf Nothing
                    Just l' -> run . search $ f l'
                Node ls -> Node <$> mapM (run . search . (>>= f) . Search . TreeT . return) ls

instance MonadTrans SearchTree where
    lift = Search . TreeT . fmap (return . Just)

instance Monad m => Alternative (SearchTree m) where
   empty = mzero
   (<|>) = mplus

instance Monad m => MonadPlus (SearchTree m ) where
    mzero = Search $ TreeT $ return $ Leaf Nothing
    mplus ma mb = Search $ TreeT $ liftM2 mplus (run $ search ma) (run $ search mb)

foundT :: Functor m => SearchTree m a -> m [a]
foundT tree = catMaybes <$> toListT ( search tree)

pruneT :: Functor m => Int -> SearchTree m a -> m [a]
pruneT maxfailures = fmap (prune maxfailures ) . run . search

-- try redo depth search from lowest depth upward
-- try till a number of fails, then jumpback
-- if depth > highest depth then jump to depth - 1
-- if previous jumpback depth > depth  jump to depth - 1
-- otherwise  jump back to previous jumpback point - 1
prune :: Int -> Tree (Maybe a ) -> [a]
prune maxfailures = go [] 0 (100000000,10000000000)
  where

    failure :: [Tree (Maybe a)] -> Int ->(Int,Int) -> [a]
    failure stack failures bounds@(previousDepth,highestDepth) =
      let depth = length stack
      in if  failures + 1 >= maxfailures
         then if
          |depth >= highestDepth  -> jumpback 1 stack (highestDepth,highestDepth)
          |depth <= previousDepth -> jumpback 1 stack (depth       ,highestDepth)
          |otherwise -> jumpback  (depth - previousDepth -1 ) stack (depth,highestDepth)
         else next stack (failures + 1) bounds

    jumpback :: Int -> [Tree (Maybe a)] -> (Int, Int) -> [a]
    jumpback jumps stack bounds = next (drop jumps stack) 0 bounds

    next [] _ _ = []
    next (nextTry : rest) failures bounds = go rest failures bounds nextTry

    go :: [Tree (Maybe a)] -> Int ->  (Int,Int) -> Tree (Maybe a) -> [a]
    go stack failures bounds tree = case tree of
        (Leaf Nothing) -> failure stack failures bounds
        (Leaf (Just a)) -> a : next stack 0 bounds
        (Node []) -> next stack failures  bounds
        (Node (l : ls)) -> go (Node ls : stack) failures  bounds l
