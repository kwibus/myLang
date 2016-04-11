module Logic where

import Test.QuickCheck.Gen
import Control.Monad.Trans.Class
import Control.Monad.State.Strict

import BackListT
import qualified BackList as B

type LogicGen s a = StateT s (BackListT Gen) a
-- first state and then Backlist else the state does not backtrack

oneOfLogic :: [LogicGen s a] -> LogicGen s a
oneOfLogic list = do
  a <- lift $ lift $ shuffle list
  tryM a

elementsLogic :: [a] -> LogicGen s a
elementsLogic list = do
  a <- lift $ lift $ shuffle list
  try a

chooseLogic :: Enum a => (a, a) -> LogicGen s a
chooseLogic (a, b) = elementsLogic [a .. b]

whenBacksteps :: Monad m => (Int -> Bool ) -> StateT s (BackListT m) a ->
  StateT s (BackListT m) a -> StateT s (BackListT m) a
whenBacksteps condition result alternative = StateT $ \ s -> BackListT $
  do bs <-run $ runStateT result s
     if condition $ B.backsteps bs
       then return bs
       else run $ runStateT alternative s

evalT :: Monad m => StateT s (BackListT m) a -> s -> m [a]
evalT a s = toListT (evalStateT a s )

liftBackList :: B.BackList a -> LogicGen s a
liftBackList bl = StateT $ \ s -> BackListT $ return $ fmap (\ i -> (i, s)) bl

try :: [a] -> LogicGen s a
try = liftBackList . B.try

tryM :: [LogicGen s a] -> LogicGen s a
tryM list = StateT $ \ s -> tryMT $ map ( \ a -> runStateT a s) list
