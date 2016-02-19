{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module ErrorCollector where

import Data.Monoid
-- import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad
import Data.Bifunctor

data ErrorCollector e a = Error e | Result a deriving (Show, Eq)

instance Functor (ErrorCollector a) where
  fmap f (Result a) = Result $ f a
  fmap _ (Error e) = Error e

instance Monoid e => Applicative (ErrorCollector e) where
  pure = return
  Result f <*> Result a = Result $ f a
  Result _ <*> Error e = Error e
  Error e1 <*> Error e2 = Error $ e1 <> e2
  Error e <*> Result _ = Error e

instance Monoid e => Monad (ErrorCollector e) where
  return = Result
  m >>= f = case m of
              Result a -> f a
              Error e -> Error e

newtype ErrorCollectorT e m a = ErrorT {runErrorT :: m (ErrorCollector e a)}

instance (Monoid e, Monad m) => Monad (ErrorCollectorT e m) where
  return = pure
  (>>) = (*>)
  m >>= f = ErrorT $ do
    errorA <- runErrorT m
    case errorA of
      Error e -> return $ Error e
      Result a -> runErrorT $ f a

instance (Applicative m , Monoid e) => Applicative (ErrorCollectorT e m) where
  pure = ErrorT . pure . Result
  fa <*> fb = ErrorT ( (<*>) <$> runErrorT fa <*> runErrorT fb)

instance Functor m => Functor (ErrorCollectorT e m) where
  fmap f ma = ErrorT $ fmap (fmap f ) (runErrorT ma)

toExcept :: (Monad m, Monoid e) => ErrorCollector e a -> ErrorCollectorT e m a
toExcept errorCol = case errorCol of
    Error e -> ErrorT $ return $ Error e 
    Result a -> return a

-- instance Monoid e => MonadError e (ErrorCollector e ) where
--   throwError = Error
--   catchError ma f = case ma of
--     Error e -> f e
--     a -> a

-- instance (Monad m, Monoid e) => MonadError e (ErrorCollectorT e m) where
--   throwError = ErrorT . return . Error
--   catchError ema f = ErrorT $ do
--     ma <- runErrorT ema
--     case ma of
--        Error e -> runErrorT $ f e
--        a -> return a

class (Monad  (m e), Bifunctor m ) => BiMonad m e where
    throw :: e -> m e a
    catch :: m e1 a -> (e1 -> m e2 a ) -> m e2 a

instance  Monoid e => BiMonad ErrorCollector e where
    throw = Error
    catch ma f = case ma of
        Error e -> f e
        Result a -> Result a

-- instance (Monad m, Monoid e) => BiMonad ErrorCollectorT e m where

throwT :: Monad m => e -> ErrorCollectorT e m a
throwT = ErrorT . return . Error

catchT :: Monad m => ErrorCollectorT  e1 m a -> ( e1 -> ErrorCollectorT e2 m a ) -> ErrorCollectorT e2 m a
catchT ema f = ErrorT $ do
    ma <- runErrorT ema
    case ma of
       Error e -> runErrorT $ f e
       Result a -> return $ Result a

-- instance (BiMonad f e a) => MonadError e (f e)  where
--     throwError = throw 
--     catchError = catch

instance MonadTrans (ErrorCollectorT e) where
  lift m = ErrorT $ do
      a <- m
      return $ Result a

instance (Monoid e, MonadState s m ) => MonadState s (ErrorCollectorT e m) where
  get = lift get
  put = lift . put
  state = lift . state

instance Bifunctor ErrorCollector where
  second = fmap
  first f (Error e) = Error $ f e
  first _ (Result a) = Result a

hasSucces :: ErrorCollector e a -> Bool
hasSucces (Error _ ) = False
hasSucces (Result _) = True

--
fromEither :: MonadPlus m => Either e a -> ErrorCollector (m e) a
fromEither (Left e) = Error $ return e
fromEither (Right a) = Result a

mapError :: Bifunctor f => (e1 -> e2 ) -> f e1 a -> f e2 a
mapError = first

toEither :: ErrorCollector e a -> Either e a
toEither (Error e ) = Left e
toEither (Result a) = Right a
