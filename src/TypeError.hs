module TypeError where

import Prelude hiding ( (<$>) )
import Text.PrettyPrint.ANSI.Leijen
import FreeEnvironment
import BruijnEnvironment
import Type
import BruijnTerm
import SourcePostion
import Lexer (toChar, reservedSymbols)

data TypeError i j =
      UnifyAp (BruijnTerm i j) Type Type [UnificationError]
    | UnifySubs (BruijnTerm i j) [UnificationError]
    | UnifyDef Type Type [UnificationError] -- TODO store more information for good error messages
    | ICE (UndefinedVar Bound j)
    deriving Show

data UnificationError =
    Infinit Free Type
  | Unify Type Type
    deriving Show

-- TODO better EqalitieA / remove and make seperate for unittest
instance Eq (TypeError i j) where
  (==) (UnifyDef t11 t12 _) (UnifyDef t21 t22 _) = t11 == t21 && t12 == t22
  (==) (UnifyAp _ _ _ err1) (UnifyAp _ _ _ err2) = err1 == err2
  (==) (UnifySubs _ _) (UnifySubs _ _ ) = True
  (==) (ICE _) (ICE _) = True
  (==) _ _ = False

instance Eq UnificationError where
   Infinit {} == Infinit {} = True
   Unify {} == Unify {} = True
   (==) _ _ = False

showErrors :: String -> [TypeError SourcePos SourcePos] -> Doc
showErrors str = vcat . map (showError str)

showError :: String -> TypeError SourcePos SourcePos -> Doc
showError str (UnifyAp expr t1 t2 err ) = text (showPosition (getPosition expr)) <+> text "TypeError " <$>
        indent 4 ( showUnifyApError str expr t1 t2 err)
showError _ _ = text "No error messages implemented"

showUnifyApError :: String -> BruijnTerm SourcePos SourcePos -> Type -> Type -> [UnificationError] -> Doc
showUnifyApError str e@(Appl e1 e2) t1 t2 _ =
  let compleetDictonarie = mkDictonarie [t1, t2]
      localShow t = text $ pShowWithDic t compleetDictonarie
  in getsource e1 str <+> text "is applied to wrong type of argumts" <$>
      dquotes (getsource e str) <$>
      indent 2 (
          getsource e1 str <+> text "::" <+> localShow t1 <$>
          getsource e2 str <+> text "::" <+> localShow t2
      )
showUnifyApError _ _ _ _ _ = text "No error messages implemented"

-- showUnifyApError str expr t1 t2(Infinit f t  env ) = undefined
-- showError str (UnifyAp Infinit  f  t env) =
    -- "can`t construct infinit Type " -- ++tShow (TVar f) ++ "= "


showLine :: Int -> Int -> String -> [String]
showLine start n str = take n $ drop (start - 1 ) $ lines str

getsource :: BruijnTerm SourcePos SourcePos -> String -> Doc
getsource term str =
    if sourceLine start /= sourceLine end
    then vsep $ map text srcLines
    else text $ tillEndColumn (sourceColumn end) $
          seekColumn (sourceColumn start)
          (head srcLines )
  where
    srcLines = showLine (sourceLine start) (sourceLine start - sourceLine end + 1) str
    end = getLastWordPos term
    start = getPosition term

seekColumn :: Int -> String -> String
seekColumn n = drop (n - 1)

tillEndColumn :: Int -> String -> String
tillEndColumn n str = begin ++ tillEndWord rest
  where
    (begin, rest) = splitAt n str

tillEndWord :: String -> String
tillEndWord [] = []
tillEndWord (s : str) = s : takeWhile (flip notElem (' ' : map toChar reservedSymbols)) str

-- trim :: String -> String
-- trim s = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s
