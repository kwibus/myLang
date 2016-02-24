module PrettyPrint
--(pShow)
where

import Text.PrettyPrint.ANSI.Leijen

import Value hiding (isInfix)
import Lambda
import Name
import Associativity
import Info
--
-- $setup
-- >>> import Operator

-- | pretty Print Lambda`s
--
-- * all printed terms can be parsed by 'Parser.parseString'.
-- * Correctly print With minimal parentheses even math expressions
--
--  examples:
--
-- >>>pShow $ Appl() (Var() (Name "a"))(Var ()(Name "b") )
-- "a b"
--
-- >>>pShow $ Lambda () (Name "a")(Var ()(Name "a") )
-- "\\a.a"
--
-- shorthand multiple lambda's
--
-- >>>pShow $ Lambda () (Name "b") (Lambda () (Name "a")(Var ()(Name "a")))
-- "\\b a.a"
--
-- 'pShow' ignores # to support infix terms that arre only apply from the left
--
-- >>> pShow $ Var () (Name "#")
-- ""
-- >>> pShow $Lambda ()(Name "#")(Var () (Name"a")   )
-- "a"
-- >>> pShow $Lambda ()(Name "#") (Appl () (Appl () (Val () plus)(Var ()(Name "#")))(Val () (MyDouble 1)))
-- "+ 1.0"

pShow :: LamTerm i Name -> String
pShow = show . go True lowPrec . removeInfo
 where
  go :: Bool                         -- ^ indicate if prented term is top leftmost of a expresion
                                     -- ^ to indicate of Lambda Terms  Should be enclosed in parenthese
      -> (Precedence, Associativity) -- ^ precedence of previous Infix  (if there is no infix then its lowPrec)
      -> LamTerm () Name             -- ^ term that should be printed
      -> Doc                         -- ^ result
  go _ _ (Var _ (Name "#")) = empty  -- ignore #
  go _ _ (Var _ (Name "##")) = empty  -- ignore #

  go _ _ (Var _ (Name n)) = text n

  go _ _ (Val _ v) = text $ pShowVal v

  go _ _ (Lambda _ (Name "##") t) = go False lowPrec t  -- ignore ##
  go topLeft _ (Lambda _ (Name "#") t) =parensIf (not topLeft)$ go True lowPrec t  -- ignore #

  go topLeft _ (Lambda _ (Name n) t) =
    let (vars, nextTerm) = accumulateVars t
    in parensIf (not topLeft) $
      backslash <>
      text ( unwords (filter (\e ->head e /= '#') (n : vars))) <>
      dot <>
      go True lowPrec nextTerm

  go topLeft p (Let _ defs term) = text "let" <+>
                                   align (vcat $ map showDefs defs) <$$>
                                   text "in" <+>
                                   go topLeft p term
    where showDefs (Def _ (Name n) t) = text (n ++ " = ") <> go True lowPrec t <> text ";"

  go topLeft p t@Appl {}
    | isInfix function = case arguments of
        [] -> docFunction
        [arg] -> myConcat [docArg False (decrement (getPrec function)) arg, docFunction]
        [arg1 , arg2 ] -> if higherPrec p (getPrec function)
                          then parens (go True lowPrec t)
                          else myConcat [docArg False (decrement (getPrec function)) arg1
                                   ,docFunction
                                   ,docArg topLeft (getPrec function ) arg2]
        (arg1 : arg2 : args) -> if higherPrec p precApplication
                                then parens $ go True lowPrec t
                                else parens (go True lowPrec (Appl () (Appl () function arg1) arg2))
                                     <+> docArgments precApplication args
    | otherwise = if higherPrec p precApplication
        then parens (go True lowPrec t)
        else nest 2 $ docFunction </> docArgments precApplication arguments
    where (function : arguments ) = accumulateArgs t
          docFunction = go False lowPrec function
          docArg topLeft' p' arg = parensIf ( isNotFullAplliedInfix arg ) $ go topLeft' p' arg
          docArgments lastPrecedence args =
            let lastArg = docArg topLeft lastPrecedence (last args)
                initArgs = map (docArg False lastPrecedence) (init args)
            in fillSep ( initArgs ++ [lastArg])

decrement :: (Precedence , Associativity) -> (Precedence , Associativity)
decrement (p, AssoLeft ) = (p, AssoRight)
decrement (p, AssoRight) = (p - 1, AssoLeft)

precApplication :: (Precedence, Associativity)
precApplication = (11, AssoLeft)

parensIf :: Bool -> Doc -> Doc
parensIf True doc = parens doc
parensIf False doc = doc

myAppend:: Doc -> Doc -> Doc
myAppend d1 d2
  | show d1 == "" = d2
  | show d2 == "" = d1
  | otherwise = d1 <+> d2

myConcat :: [Doc] -> Doc
myConcat = foldl1 myAppend

isNotFullAplliedInfix :: LamTerm i Name -> Bool
isNotFullAplliedInfix (Appl _ t1 _) = isInfix t1
isNotFullAplliedInfix t = isInfix t

accumulateVars :: LamTerm i Name -> ([String], LamTerm i Name)
accumulateVars = go []
 where go ns (Lambda _ (Name n) t ) = go (n : ns) t
       go ns t = (reverse ns, t)

accumulateArgs :: LamTerm i n -> [LamTerm i n]
accumulateArgs = go []
  where go accuList (Appl _ t1 t2 ) = go (t2 : accuList) t1
        go accuList t = t : accuList
