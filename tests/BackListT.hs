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
         Failures i -> return $ Failures i
         List (l:ls) next -> do
                 b' <- run $ f l
                 next' <- run $ BackListT (return (List ls next)) >>= f
                 return $ setTop (Just next') b'
         List  [] (Just next) -> run $ ( BackListT $ return next) >>= f
         List  [] Nothing  -> return $ Failures 0

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

failuresT :: Functor m => BackListT m a -> m Int
failuresT b = fmap failures $! run b
