module Logic where

import Test.QuickCheck.Gen
import Control.Monad.State.Strict

type LogicGen s m a = StateT s (m Gen) a

oneOfLogic :: (MonadTrans m, MonadPlus (m Gen)) =>
    [LogicGen s m a] -> LogicGen s m a
oneOfLogic list = do
  a <- lift $ lift $ shuffle list
  tryM a

elementsLogic :: (MonadTrans m,MonadPlus (m Gen)) =>
    [a] -> LogicGen s m a
elementsLogic list = do
  a <- lift $ lift $ shuffle list
  try a

chooseLogic ::  (MonadTrans m,MonadPlus (m Gen)) =>
    Enum a => (a, a) -> LogicGen s m a
chooseLogic (a, b) = elementsLogic [a .. b]

try :: MonadPlus m => [a] -> m a
try = tryM . map return

-- can't user msum appends mzero at the end
-- and searchtree is not a currect monadplus (only if you observe it via found prune)
tryM ::MonadPlus m => [m a] ->m a
tryM [] = mzero
tryM list = foldl1 mplus  list
