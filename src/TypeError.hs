module TypeError where
import Prelude hiding ( (<$>) )
import Text.PrettyPrint.ANSI.Leijen
import Enviroment
import Type
import BruijnTerm
import Lambda
import Info
import Control.Monad.State

import Text.Parsec.Pos

data TypeError i =
      UnifyAp  (BruijnTerm i) (UnificationError i)
    | UnifyEnv (BruijnTerm i) [UnificationError i] 
    | ICE (UndefinedVar i)
    deriving Show

data UnificationError i =
    Infinit ( Free) (Type Free) (FreeEnv (Type Free))
  | Unify ( Type Free) (Type Free) (FreeEnv ( Type Free))
  | VarVar
    deriving Show

-- TODO better EqalitieA / remove and make seperate for unittest 
instance Eq (TypeError i) where
  (==) (UnifyAp _ _) (UnifyAp _ _) = True
  (==) (UnifyEnv _ _) (UnifyEnv _ _ ) = True
  (==) (ICE _) (ICE _) = True
  (==) _ _ = False

showError :: String -> (TypeError Loc) -> Doc 
showError str (UnifyAp exp error ) = showUnifyApError str exp  error

showUnifyApError :: String -> BruijnTerm Loc -> UnificationError Loc -> Doc
showUnifyApError str (Appl i e1 e2)  (Unify t1 t2 env) = 
    let namestate = genNames t1 >> genNames t2
        localShow t = text $ evalState (namestate >> tShowEnv t) initState
    in getWord i  str <+> text "is applied to wrong kind of argumts"  <$> 
      text "at" <+> text (show i) <$>
      indent 4 (getWord (getposition e1) str <+> text"::"<+> localShow t1 <$>
                getWord (getposition e2) str <+> text"::"<+> localShow t2 )
-- showError str (UnifyAp Infinit  f  t env) =
    -- "can`t construct infinit Type " -- ++tShow (TVar f) ++ "= "


showLine :: Loc -> String -> String
showLine loc str = (lines str) !! (sourceLine loc - 1 )

underlineWord :: Loc -> String -> Doc
underlineWord loc str =  text pre <> red ( text word) <> ( text post )
  where  
    line = (showLine loc str)
    (pre, xs) = splitAt (sourceColumn loc) line
    (word, post) = break (\ a -> a == ' ') xs

getWord :: Loc -> String -> Doc 
getWord loc str = text word
  where 
    line = showLine loc str 
    word = takeWhile (\ a -> a /= ' ') $drop (sourceColumn loc-1 ) line 


