{-# LANGUAGE UndecidableInstances, FlexibleInstances,MultiParamTypeClasses #-}
module StateTransMany where

import Control.Monad.Identity
import Control.Monad.State
import Control.Applicative

type StateTransMany s a = StateTransManyT s Identity a

newtype StateTransManyT s m a = STM { run :: s -> m [(a, s)] }

instance Monad m => Functor (StateTransManyT s m) where
    fmap f m = liftM f m

instance (Monad m) => Applicative (StateTransManyT s m) where
    pure a = return a
    f <*> a = ap f a

instance Monad m => Monad (StateTransManyT s m) where
    return a = STM $ \ s -> return [(a, s)]
    m >>= f = STM $ \ s -> do
        l <- run m s
        let g (a1, s1) = run (f a1) s1
        liftM concat (mapM g l)

instance Monad m => MonadState s (StateTransManyT s m) where
    put s = STM (\ _ -> return [((), s)])
    get = STM (\ s -> return [(s, s)])
    state f = STM (\ s -> return [f s] )

instance Monad m => MonadPlus (StateTransManyT s m) where
    mzero = STM $ const $ return []
    mplus a b = STM $ \ s -> do
         a' <- run a s
         b' <- run b s
         return $ a' ++ b'

instance (Monad m , Applicative m) => Alternative (StateTransManyT s m) where
    empty = mzero
    (<|>) = mplus

instance MonadTrans (StateTransManyT s ) where
    lift m = STM $ \ s -> do
        a <- m
        return [(a, s)]

evalT :: Functor m => StateTransManyT s m a -> s -> m [a]
evalT e s = fmap (map fst) $ (run e s)

eval :: StateTransMany s a -> s -> [a]
eval e s = map fst $ runIdentity (run e s)
