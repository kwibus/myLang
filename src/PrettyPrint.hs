module PrettyPrint (pShow)
where

import Value hiding (isInfix)
import Lambda
import Name
import Associativity

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

-- TODO ad Show trick or pretty print library
pShow :: LamTerm i Name -> String
pShow = go False lowPrec
 where
  go :: Bool                         -- ^ to indicate of Lambda Terms  Should be enclosed in parenthese
      -> (Precedence, Associativity) -- ^ precedence of previous Infix  (if there is no infix then its lowPrec)
      -> LamTerm i Name              -- ^ term that should be printed
      -> String                      -- ^ result
  go _ _ (Var _ (Name "#")) = "" -- ignore #
  go _ _ (Var _ (Name n)) = n

  go _ _ (Val _ v) = pShowVal v

  go b _ (Lambda _ (Name "#") t) = go b lowPrec t  -- ignore #
  go b p (Lambda _ (Name n) t) =
    let (vars, nextTerm) = accumulateVars t
    in parensIf b $ "\\" ++ unwords (filter (/= "#") (n : vars)) ++ "." ++ go b p nextTerm

  go b p (Appl _ t1 t2 )
    | isInfix t1 =
        let string2 = mkString2 True (decrement (getPrecFuntion t1))
        in myConcat string2 string1

    | otherwise = parensIf ( higherPrec p (getPrecFuntion t1)) $
        let string2 = mkString2 b (getPrecFuntion t1)
        in myConcat string1 string2

    where string1 = go True presAplicationfuction t1
          mkString2 b' p' = parensIf (isNotFullAplliedInfix t2) $ go b' p' t2


getPrecFuntion :: LamTerm i n -> (Precedence, Associativity)
getPrecFuntion (Appl _ (Appl {}) _) = highPrec
getPrecFuntion (Appl _ term _ ) = getPrec term
getPrecFuntion term = getPrec term

decrement :: (Precedence , Associativity) -> (Precedence , Associativity)
decrement (p, AssoLeft ) = (p, AssoRight)
decrement (p, AssoRight) = (p - 1, AssoLeft)

presAplicationfuction :: (Precedence, Associativity)
presAplicationfuction = (11, AssoLeft)

parensIf :: Bool -> String -> String
parensIf True string = parens string
parensIf False string = string

parens :: String -> String
parens string = "(" ++ string ++ ")"

myConcat :: String -> String -> String
myConcat s "" = s
myConcat "" s = s
myConcat s1 s2 = s1 ++ " " ++ s2

isNotFullAplliedInfix :: LamTerm i Name -> Bool
isNotFullAplliedInfix (Appl _ (Appl _ t1 _ ) _) = not (isInfix t1)
isNotFullAplliedInfix (Appl _ t1 _) = isInfix t1
isNotFullAplliedInfix t = isInfix t

accumulateVars :: LamTerm i Name -> ([String], LamTerm i Name)
accumulateVars = go []
 where go ns (Lambda _ (Name n) t ) = go (n : ns) t
       go ns t = (reverse ns, t)
