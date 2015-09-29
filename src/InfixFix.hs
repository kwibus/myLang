module InfixFix
  ( fixInfix
  , InfixError (MultipleInfix)
  ) where

import Lambda
import Info
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
fixInfix expresions = case reverse <$> shuntingYard expresions [] [] of
    exprs @ (Right (Appl _ _ (Var pos (Name "#")) : _)) -> Lambda pos (Name "#") <$> (fixStream <$> exprs)
    exprs -> fixStream <$> exprs
    where fixStream :: [Expresion] -> Expresion
          fixStream = foldl1 (\ e1 e2 -> Appl (mergLoc e1 e2) e1 e2 )

-- |Modifyd ShuntingYard is a algoritme to convert infix to revers polish notation
shuntingYard ::
    [(Expresion, Bool)] -- ^ input stream annotated with Bool that is True for infix/operators
  -- (you need a bool because all Val + are not Infix because ic can be from this input (+)
  -> [Expresion] -- ^ value end normal functions stack
  -> [Expresion] -- ^ operator stack
  -> Either InfixError [Expresion] -- ^ either error od reverse polish notation
shuntingYard [] vs op = return $ fst $ unwindStack vs op
--
-- The program crash if operator is followed by a operator.
-- So you can only have one operator on the stack and one variables on the
-- stack if you end or start with a operator.
-- but in this case it is not end with a operator because the stream is non empty
-- So it`s operator only apply from the left "(+1..)".
-- So you applied from the right with a dummy variable # and put a Lambda # in front of it
-- when you have the full term (\#.#+1..).
-- Here you have to apply with #, 'fixInfix' we corrects for #.
shuntingYard (e : es) [] [o] =
    let pos = getLocation o
    in shuntingYard (e : es) [Appl pos o (Var pos (Name "#"))] []

-- when you find "+ *" it  cant differentiate (+) * or + (*)
shuntingYard ((e1, True) : (e2, True) : _) _ _ = Left $ MultipleInfix e1 e2
shuntingYard ((e, True) : es) vs op = if higherThenTop e op
            then let (vs1, op1) = unwindStack vs op
                 in shuntingYard es vs1 (e : op1)
            else shuntingYard es vs (e : op)
--
-- when you find to non operators next to each other ("f a").
-- then the first one is a function and you want to apply function to its argents.
-- the result of this is a value so push it on the vallue stack.
shuntingYard ((e, False) : es) vs op =
    let (functionsAndArgs, rest) = break snd es
        nonInfix = foldl1 (\ e1 e2 -> Appl (mergLoc e1 e2) e1 e2) (e : map fst functionsAndArgs)
    in shuntingYard rest (nonInfix : vs) op

unwindStack :: [Expresion] -> [Expresion] -> ([Expresion], [Expresion])
unwindStack [] os = (os, [])
unwindStack vs [] = (vs, [])
unwindStack (v1 : v2 : vs) (o : os) =
    let pos = getLocation v2
    in uncurry unwindStack (Appl pos (Appl pos o v2) v1 : vs, os)
unwindStack (v : vs) (o : os) = (Appl (getLocation v) o v : vs, os)

higherThenTop :: Expresion -> [Expresion] -> Bool
higherThenTop _ [] = True
-- 'higherPrec'  accept expresion in the order they appear; so stack expression first
higherThenTop y (x : _) = higherPrec (getPrec x) (getPrec y)
