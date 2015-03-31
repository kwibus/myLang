{-# LANGUAGE UndecidableInstances, FlexibleInstances,MultiParamTypeClasses #-}
module StateTransMany where

import Control.Monad.Identity
import Control.Monad.State
import Control.Arrow
import Control.Applicative

type StateTransMany s a = StateTransManyT s Identity a

newtype StateTransManyT s m a = STM { run :: s -> m [(a, s)] }

instance Functor m => Functor (StateTransManyT s m) where
    fmap f m = STM $ \ s -> fmap (map (first f)) $ run m s

concatMapM :: Monad m => (a -> m [b] ) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

concatMapM2 :: Monad m => (a -> m [b] ) -> m [a] -> m [b]
concatMapM2 f xs = do
    as <- xs
    b <- mapM f as
    return $ concat b

instance (Monad m, Functor m) => Applicative (StateTransManyT s m) where
    pure a = return a
    f <*> a = STM $ \ s ->
        let f1 = run f s
        in concatMapM2 (\ (f2, s1) ->
             fmap (map (first f2)) $ run a s1
        ) f1
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
