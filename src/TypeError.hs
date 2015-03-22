module TypeError where

import Enviroment
import Type
import BruijnTerm

data TypeError i =
      Infinit (i, Free) (i, Type Free) (FreeEnv (i, Type Free))
    | Unify (i, Type Free) (i, Type Free) (FreeEnv (i, Type Free))
    | ICE (UndefinedVar i)
    | VarVar

-- TODO better Eqalitie
instance Eq (TypeError i) where
  (==) (Unify _ _ _) (Unify _ _ _) = True
  (==) VarVar VarVar = True
  (==) (ICE _) (ICE _) = True
  (==) (Infinit _ _ _) (Infinit _ _ _) = True
  (==) _ _ = False

instance Show (TypeError i) where
    show (Infinit (i1,f) (i2,t) env) = "can`t construct infinit Type " -- ++tShow (TVar f) ++ "= " 
