module MakeType where

import Type
import FreeEnvironment

tVar :: Int -> Type
tVar  = TVar . Free
