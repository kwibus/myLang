module InfixFix (fixInfix, InFixError) where

import Expresion (Expresion)
import Lambda
import Vallue
import Info
import Names

data InFixError = MultipleInfix Expresion Expresion
    deriving (Show, Eq)

fixInfix :: [(Expresion, Bool)] -> Either InFixError [Expresion]
fixInfix e = reverse <$> fixInfix1 e [] []

-- TODO beter error messages
-- TODO beter name
fixInfix1 :: [(Expresion, Bool)] -> [Expresion] -> [Expresion] -> Either InFixError [Expresion]
fixInfix1 [] vs op = return $ fst $ unwindStacks vs op
fixInfix1 (e : es) [] [o] =
    let pos = getposition o
    in fixInfix1 (e : es) [Lambda pos (Name "#") (Appl pos o (Var pos (Name "#")))] []
fixInfix1 ((e1, True) : (e2, True) : _) _ _ = Left $ MultipleInfix e1 e2
fixInfix1 ((e, True) : es) vs op = if higer op e
            then let (vs1, op1) = unwindStacks vs op
                 in fixInfix1 es vs1 (e : op1)
            else fixInfix1 es vs (e : op)
fixInfix1 ((e, False) : es) vs op = fixInfix1 es (e : vs) op

unwindStacks :: [Expresion] -> [Expresion] -> ([Expresion], [Expresion])
unwindStacks [] os = (os, [])
unwindStacks vs [] = (vs, [])
unwindStacks (v1 : v2 : vs) (o : os) =
    let pos = getposition v2
    in uncurry unwindStacks (Appl pos (Appl pos o v2) v1 : vs, os)
unwindStacks (v : vs) (o : os) = (Appl (getposition v) o v : vs, os)

getpres :: Expresion -> (Precedence, Associativity)
getpres (Val _ BuildIn {fixity = InFix p a}) = (p, a)
getpres _ = error "no infix"

-- TODO correct associative
higer :: [Expresion] -> Expresion -> Bool
higer [] _ = True
higer (x : _) y = higerPres (getpres x) (getpres y)

higerPres :: (Precedence, Associativity) -> (Precedence, Associativity) -> Bool
higerPres (p1, AssoLeft) (p2, _) = p1 >= p2
higerPres (p1, AssoRight) (p2, _) = p1 > p2
