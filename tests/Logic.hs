module Logic where

import Test.QuickCheck.Gen
import Control.Monad
import Control.Monad.Trans.Class
import StateTransMany

oneOfLogic :: [StateTransManyT s Gen a] -> StateTransManyT s Gen a
oneOfLogic list = do
    a <- lift $ shuffle list
    msum a

elementsLogic :: [a] -> StateTransManyT s Gen a
elementsLogic list = do
     a <- lift $ shuffle list
     msum $ map return a

chooseLogic :: Enum a => (a, a) -> StateTransManyT s Gen a
chooseLogic (a, b) = elementsLogic [a .. b]
