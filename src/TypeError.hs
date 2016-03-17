module TypeError where

import Prelude hiding ( (<$>) )
import Text.PrettyPrint.ANSI.Leijen
import Data.Char

import FreeEnvironment
import BruijnEnvironment
import Type
import BruijnTerm
import Lambda
import Info

data TypeError i =
      UnifyAp (BruijnTerm i) Type Type [UnificationError]
    | UnifySubs (BruijnTerm i) [UnificationError]
    | ICE (UndefinedVar Bound i)
    | Dummy1 -- FIXME
    | Dummy2 -- FIXME
    | Dummy3 -- FIXME
    deriving Show

data UnificationError =
    Infinit Free Type
  | Unify Type Type
    deriving Show

-- TODO better EqalitieA / remove and make seperate for unittest
instance Eq (TypeError i) where
  (==) (UnifyAp _ _ _ err1) (UnifyAp _ _ _ err2) = err1 == err2
  (==) (UnifySubs _ _) (UnifySubs _ _ ) = True
  (==) (ICE _) (ICE _) = True
  (==) _ _ = False

instance Eq UnificationError where
   Infinit {} == Infinit {} = True
   Unify {} == Unify {} = True
   (==) _ _ = False

showErrors :: String -> [TypeError Loc] -> Doc
showErrors str = vcat . map (showError str)

showError :: String -> TypeError Loc -> Doc
showError str (UnifyAp expr t1 t2 err ) = text (showLoc (getLocation expr)) <+> text "TypeError " <$>
        indent 4 ( showUnifyApError str expr t1 t2 err)
showError _ _ = text "No error messages implemented"

showUnifyApError :: String -> BruijnTerm Loc -> Type -> Type -> [UnificationError] -> Doc
showUnifyApError str (Appl i e1 e2) t1 t2 _ =
  let compleetDictonarie = mkDictonarie [t1, t2]
      localShow t = text $ pShowWithDic t compleetDictonarie
  in getWord (getLocation e1) str <+> text "is applied to wrong type of argumts" <$>
      dquotes (getWord i str) <$>
      indent 2 (
          getWord (getLocation e1) str <+> text "::" <+> localShow t1 <$>
          getWord (getLocation e2) str <+> text "::" <+> localShow t2
      )
showUnifyApError _ _ _ _ _ = text "No error messages implemented"

-- showUnifyApError str expr t1 t2(Infinit f t  env ) = undefined
-- showError str (UnifyAp Infinit  f  t env) =
    -- "can`t construct infinit Type " -- ++tShow (TVar f) ++ "= "


showLine :: Loc -> String -> String
showLine loc str = lines str !! (lineStart loc - 1 )

underlineWord :: Loc -> String -> Doc
underlineWord loc str = text pre <> red ( text word) <> text post
  where
    selectedLine = showLine loc str
    (pre, xs) = splitAt (columnStart loc) selectedLine
    (word, post) = break (== ' ') xs

getWord :: Loc -> String -> Doc
getWord loc str =
    if lineStart loc /= lineEnd loc
    then vsep $ map text $ take (lineEnd loc - lineStart loc + 1) $ drop (lineStart loc - 1) (lines str)
    else text $ trim word
  where
    selectedLine = showLine loc str
    word = take (columnEnd loc - columnStart loc ) $ drop (columnStart loc - 1 ) selectedLine

trim :: String -> String
trim s = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s
