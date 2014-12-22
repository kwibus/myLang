module Vallue where

import Control.Monad.State.Strict
type Stack = [Vallue]

data Vallue = MyDouble Double
    | BuildIn { name :: String
             , arrity :: Int
             , evaluator :: State Stack Vallue
             , stack :: Stack}

instance Show Vallue where
    show (MyDouble a) = show a
    show BuildIn {name = n} = n

instance Eq Vallue where
    (==) (MyDouble a) (MyDouble b) = abs (a - b) < 0.0001
