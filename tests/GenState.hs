module GenState where

import Control.Monad

import Enviroment

data GenState = State
    { freeNames :: [String]
    , dictionary :: BruiEnv (String, Free)
    }

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

defualtGenState :: GenState
defualtGenState = State { -- names = []
                        dictionary = bEmtyEnv
                        , freeNames = letters
                        }
