module TypeError where

import Text.PrettyPrint.ANSI.Leijen
import Enviroment
import Type
import BruijnTerm
import Info
import Control.Monad.State

import Text.Parsec.Pos

data TypeError i =
      Infinit ( Free) (Type Free) (FreeEnv (Type Free))
    | Unify ( Type Free) (Type Free) (FreeEnv ( Type Free))
    | UnifyEnv [TypeError i] 
    | ICE (UndefinedVar i)
    | VarVar deriving Show

-- TODO better EqalitieA / remove and make seperate for unittest 
instance Eq (TypeError i) where
  (==) (Unify _ _ _) (Unify _ _ _) = True
  (==) VarVar VarVar = True
  (==) (ICE _) (ICE _) = True
  (==) (Infinit _ _ _) (Infinit _ _ _) = True
  (==) _ _ = False

showError :: String -> (TypeError Loc) -> String
showError str (Infinit  f  t env) =
    "can`t construct infinit Type " -- ++tShow (TVar f) ++ "= "
showError str (Unify t1 t2 env) =
    let namestate = genNames t1 >> genNames t2
        localShow t = evalState (namestate >> tShowEnv t) initState
    in "can`t unify " ++ localShow t1 ++ " from " ++ -- showLine i1 str ++
       "\nwith " ++ localShow t2

showLine :: Loc -> String -> String
showLine loc str = (lines str) !! (sourceLine loc - 1 )

underlineWord :: Loc -> String -> Doc
underlineWord loc str =  text pre <> red ( text word) <> ( text post )
  where  
    line = (showLine loc str)
    (pre, xs) = splitAt (sourceColumn loc) line
    (word, post) = break (\ a -> a == ' ') xs

getWord :: Loc -> String -> String
getWord loc str = word
  where 
    line = showLine loc str 
    word = takeWhile (\ a -> a /= ' ') $drop (sourceColumn loc-1 ) line 


