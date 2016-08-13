module PrettyPrint
--(pShow)
where

import Text.PrettyPrint.ANSI.Leijen

import Value hiding (isInfix)
import Lambda
import Name
import Associativity

-- $setup
-- >>> import Operator

-- TODO maybe use MakeTerm
-- | pretty Print Lambda`s
--
-- * all printed terms can be parsed by 'Parser.parseString'.
-- * Correctly print With minimal parentheses even math expressions
--
--  examples:
--
-- >>>pShow ( Appl (Var() (Name "a"))(Var ()(Name "b") ) :: LamTerm Name () Name )
-- "a b"
--
-- >>>pShow $ Lambda (Name "a")(Var ()(Name "a") )
-- "\\a.a"
--
-- shorthand multiple lambda's
--
-- >>>pShow $ Lambda (Name "b") (Lambda (Name "a")(Var ()(Name "a")))
-- "\\b a.a"
--
-- 'pShow' ignores DummyBegin to support infix terms that arre only apply from the left
-- 'pShow' ignores DummyEnd to support infix terms that arre only apply from the Right
-- >>> pShow ( Var () DummyBegin :: LamTerm Name () Name )
-- ""
--
-- >>> pShow $Lambda (DummyBegin)(Var () (Name"a")   )
-- "a"
--
-- >>> pShow $Lambda (DummyBegin) (Appl  (Appl (Lit() plus)(Var ()DummyBegin))(Lit() (Prim $ MyDouble 1)))
-- "+ 1.0"

pShow ::HasName lam => LamTerm lam i Name -> String
pShow = show . go True lowPrec
 where
  go :: HasName lam => Bool                         -- ^ indicate if prented term is top leftmost of a expresion
                                     -- ^ to indicate of Lambda Terms  Should be enclosed in parenthese
      -> (Precedence, Associativity) -- ^ precedence of previous Infix  (if there is no infix then its lowPrec)
      -> LamTerm lam i Name             -- ^ term that should be printed
      -> Doc                         -- ^ result
  go _ _ (Var _ name ) = text $ prettyPrint name -- ignore Empty

  go _ _ (Lit _ v) = text $ pShowVal v

  go topLeft _ e@Lambda {} =
    let (vars, nextTerm) = accumulateVars e
        lambda = if null vars then text ""
                 else backslash <>
                    text ( unwords ( fmap toString vars)) <>
                    dot
    in parensIf (not topLeft) $lambda <> go True lowPrec nextTerm

  go topLeft p (Let _ defs term) = text "let" <+>
                                   align (vcat $ map showDefs defs) <$$>
                                   text "in" <+>
                                   go topLeft p term
    where showDefs (Def _ n t) = text (toString n ++ " = ") <> go True lowPrec t <> text ";"

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
                                else parens (go True lowPrec (Appl (Appl function arg1) arg2))
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

isNotFullAplliedInfix :: LamTerm lam i Name -> Bool
isNotFullAplliedInfix (Appl t1 _) = isInfix t1
isNotFullAplliedInfix t = isInfix t

