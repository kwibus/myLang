module BackListT where

import Control.Monad.Trans.Class
import Control.Monad
import Control.Applicative

import BackList

newtype BackListT m a = BackListT {run :: m (BackList a)}

instance Monad m => Functor (BackListT m) where
  fmap = liftM

instance Monad m => Applicative (BackListT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (BackListT m ) where
  return a = BackListT $ return $ return a
  m >>= f = BackListT $ do
      b <- run m
      case b of
          Steps i -> return $ Steps i
          List [] -> error "to low"
          List [l] -> run $ f l
          List ls -> run (tryMT (map f ls))

instance Monad m => MonadPlus (BackListT m) where
  mzero = BackListT $ return mzero
  mplus a b = BackListT $ do
      a' <- run a
      b' <- run b
      return $ mplus a' b'

instance Monad m => Alternative (BackListT m) where
   empty = mzero
   (<|>) = mplus

instance MonadTrans BackListT where
  lift = BackListT . fmap return

tryT :: Monad m => [a] -> BackListT m a
tryT = BackListT . return . try

tryMT :: Monad m => [BackListT m a] -> BackListT m a
tryMT list = BackListT (tryM <$> mapM run list)

toListT :: Functor m => BackListT m a -> m [a]
toListT = fmap toList . run

backstepsT :: Functor m => BackListT m a -> m Int
backstepsT b = fmap backsteps $! run b

stepsT :: Monad m => Int -> BackListT m a
stepsT = BackListT . return . Steps
