module GenState where

import Names
import Enviroment

data GenState = State
    { freeNames :: [String]
    , dictionary :: BruiEnv (String, ((), Free))
    }


defualtGenState :: GenState
defualtGenState = State { dictionary = bEmtyEnv
                        , freeNames = letters
                        }
