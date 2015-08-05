module InfixFix (fixInfix, InfixError) where

import PrettyPrint
import Lambda
import Info
import Name
import Associativity

data InfixError = MultipleInfix Expresion Expresion
    deriving (Show, Eq)

fixInfix :: [(Expresion, Bool)] -> Either InfixError Expresion
fixInfix expresions = case reverse <$> fixInfix1 expresions [] [] of
    exprs @ (Right (Appl _ _ (Var pos (Name "#")) : _)) -> Lambda pos (Name "#") <$> (fixStream <$> exprs)
    exprs -> fixStream <$> exprs
    where fixStream :: [Expresion] -> Expresion
          fixStream = foldl1 (\ e1 e2 -> Appl (mergLoc e1 e2) e1 e2 )

-- TODO beter name
fixInfix1 :: [(Expresion, Bool)] -> [Expresion] -> [Expresion] -> Either InfixError [Expresion]
fixInfix1 [] vs op = return $ fst $ unwindStack vs op
fixInfix1 (e : es) [] [o] =
    let pos = getLocation o
    in fixInfix1 (e : es) [Appl pos o (Var pos (Name "#"))] []
fixInfix1 ((e1, True) : (e2, True) : _) _ _ = Left $ MultipleInfix e1 e2
fixInfix1 ((e, True) : es) vs op = if higher op e
            then let (vs1, op1) = unwindStack vs op
                 in fixInfix1 es vs1 (e : op1)
            else fixInfix1 es vs (e : op)
fixInfix1 ((e, False) : es) vs op =
    let (functionsAndArgs, rest) = break snd es
        nonInfix = foldl1 (\ e1 e2 -> Appl (mergLoc e1 e2) e1 e2) (e : map fst functionsAndArgs) :: Expresion
    in fixInfix1 rest (nonInfix : vs) op

unwindStack :: [Expresion] -> [Expresion] -> ([Expresion], [Expresion])
unwindStack [] os = (os, [])
unwindStack vs [] = (vs, [])
unwindStack (v1 : v2 : vs) (o : os) =
    let pos = getLocation v2
    in uncurry unwindStack (Appl pos (Appl pos o v2) v1 : vs, os)
unwindStack (v : vs) (o : os) = (Appl (getLocation v) o v : vs, os)

higher :: [Expresion] -> Expresion -> Bool
higher [] _ = True
higher (x : _) y = higherPrec (getPrec x) (getPrec y)
