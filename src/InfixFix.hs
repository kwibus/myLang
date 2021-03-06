module InfixFix
  ( fixInfix
  , InfixError (MultipleInfix)
  ) where

import Lambda
import SourcePostion
import Name
import Associativity

-- | Possible errors that fixity can generate.
data InfixError = MultipleInfix Expresion Expresion
                -- ^ detect two infix terms next to each other
    deriving (Show, Eq)

-- | Transforms a stream of terms in the order the appear,
-- and transforms them to a term where the arre applied in the correct order.
-- it fails if you have 2 infix terms next to each other,
-- because when it get "+ *" it cant differentiate it with "(+) *" or "+ (*)"

fixInfix :: [(Expresion, Bool)] -- ^ List of terms in the order the appear.
    -> Either InfixError Expresion -- ^ Invalid stream or term with correct order.
fixInfix expresions = case expresions of
    -- infix that are only applied from the left are replaced with:
    -- (+1..) == \a.(a+1..)
    ((e, True) : _ : _) -> Lambda (getPosition e) DummyBegin <$>
            inserMissingLastVar ((Var (getPosition e) DummyBegin, False) : expresions)
    _ -> inserMissingLastVar expresions
    where list2Appl :: [Expresion] -> Expresion
          list2Appl = foldl1 Appl
          -- shuntingYard does not work with not fully applied infix
          -- so append variable if last expresion is infix and prepend with lambda for that variable
          inserMissingLastVar :: [(Expresion, Bool)] -> Either InfixError Expresion
          inserMissingLastVar stream = case last stream of
               (_, False) -> toPolishNotation stream
               (lastE, True) -> fixEndingHiddenVariable <$> toPolishNotation
                   (stream ++ [(Var (getPosition lastE) DummyEnd, False)])
          toPolishNotation :: [(Expresion, Bool)] -> Either InfixError Expresion
          toPolishNotation stream = list2Appl . reverse <$> shuntingYard stream [] []
          -- append variable can be remove via eta conversion
          -- for eta conversion you normally have to consider if the removed variable is used in the body
          -- but now you don't have to consider that, because its only appended on the end
          fixEndingHiddenVariable :: Expresion -> Expresion
          fixEndingHiddenVariable (Appl e (Var _ DummyEnd)) = e
          fixEndingHiddenVariable e = Lambda (getPosition e) DummyEnd e

-- |Modifyd ShuntingYard is a algoritme to convert infix to revers polish notation
shuntingYard ::
    [(Expresion, Bool)] -- ^ input stream annotated with Bool that is True for infix/operators
  -- (you need a bool because all Val + are not Infix because ic can be from this input (+)
  -> [Expresion] -- ^ value end normal functions stack
  -> [Expresion] -- ^ operator stack
  -> Either InfixError [Expresion] -- ^ either error od reverse polish notation

shuntingYard [] vs op = return $ fst $ unwindStack vs op

-- when you find "+ *" it  cant differentiate (+) * or + (*)
shuntingYard ((e1, True) : (e2, True) : _) _ _ = Left $ MultipleInfix e1 e2
shuntingYard ((e, True) : es) vs op = if higherThenTop e op
            then let (vs1, op1) = unwindStack vs op
                 in shuntingYard es vs1 (e : op1)
            else shuntingYard es vs (e : op)

-- when you find to non operators next to each other ("f a").
-- then the first one is a function and you want to apply function to its argents.
-- the result of this is a value so push it on the value stack.
shuntingYard ((e, False) : es) vs op =
    let (functionsAndArgs, rest) = break snd es
        nonInfix = foldl1 Appl (e : map fst functionsAndArgs)
    in shuntingYard rest (nonInfix : vs) op

unwindStack :: [Expresion] -> [Expresion] -> ([Expresion], [Expresion])
unwindStack [] os = (os, [])
unwindStack vs [] = (vs, [])
unwindStack (v1 : v2 : vs) (o : os) = uncurry unwindStack (Appl (Appl o v2) v1 : vs, os)
unwindStack (v : vs) (o : os) = (Appl o v : vs, os)

higherThenTop :: Expresion -> [Expresion] -> Bool
higherThenTop _ [] = True
-- 'higherPrec'  accept expresion in the order they appear; so stack expression first
higherThenTop y (x : _) = higherPrec (getPrec x) (getPrec y)
