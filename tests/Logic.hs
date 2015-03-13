module Logic where


import Test.QuickCheck.Gen
import Control.Monad
import Control.Monad.Logic
-- import Debug.Trace

oneOfLogic :: [LogicT Gen a] -> LogicT Gen a
oneOfLogic list = mychoose (length list) list
    where mychoose _ [] = mzero
          mychoose n l
            | n > 1000 = mzero
            | otherwise = do
              i <- lift $ choose (0, n - 1)
              let (result , rest) = extract i l
              result `mplus` mychoose (n - 1) rest

elementsLogic :: [a] -> LogicT Gen a
elementsLogic list = mychoose (length list) list
    where mychoose _ [] = mzero
          mychoose n l
            | n > 1000 = mzero
            | otherwise = do
              i <- lift $ choose (0, n - 1)
              let (result , rest) = extract i l
              return result `mplus` mychoose (n - 1) rest

chooseLogic :: Enum a => (a, a) -> LogicT Gen a
chooseLogic (a, b) = elementsLogic [a .. b]

extract :: Int -> [a] -> (a, [a])
extract n list = go n list []
    where go 0 (r : rs) l = (r, rs ++ l)
          go _ [] _ = error "not in yo"
          go i (r : rs) l = go (i - 1) rs (r : l)
