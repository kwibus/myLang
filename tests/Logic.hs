module Logic where


import Test.QuickCheck.Gen
import Control.Monad
import Control.Monad.Logic
-- import Debug.Trace

oneOfLogic :: [LogicT Gen a] -> LogicT Gen a
oneOfLogic list = do
    a <- lift $ shuffle list
    msum a

elementsLogic :: [a] -> LogicT Gen a
elementsLogic list = do
     a <- lift $ shuffle list
     msum $ map return a

chooseLogic :: Enum a => (a, a) -> LogicT Gen a
chooseLogic (a, b) = elementsLogic [a .. b]

extract :: Int -> [a] -> (a, [a])
extract n list = go n list []
    where go 0 (r : rs) l = (r, rs ++ l)
          go _ [] _ = error "not in yo"
          go i (r : rs) l = go (i - 1) rs (r : l)
