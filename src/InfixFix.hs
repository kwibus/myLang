module InfixFix (fixInfix,fixInfix1) where

import Expresion (Expresion)
import Lambda
import Vallue
import Info

fixInfix :: [(Expresion,Bool)] -> [Expresion]
fixInfix e = reverse $ fixInfix1  e [] []

-- TODO beter error messages
fixInfix1 ::  [(Expresion,Bool)] -> [Expresion] -> [Expresion]-> [Expresion]
fixInfix1 [] vs op = fst $ unwindStacks  vs op
fixInfix1 (e:es) [] [o] =
    let pos = getposition o
    in fixInfix1 (e:es) [Lambda pos "#" (Appl pos o (Var pos "#"))] []
fixInfix1 ((e ,True):es) vs op = if higer op e
            then let (vs1,op1)=unwindStacks vs op 
                 in fixInfix1 es vs1 (e :op1) 
            else fixInfix1 es vs (e:op) 
fixInfix1 ((e ,False):es) vs op = fixInfix1 es (e:vs) op

unwindStacks :: [Expresion] -> [Expresion] -> ([Expresion],[Expresion])
unwindStacks [] os = (os, [])
unwindStacks vs [] = (vs, [])
unwindStacks (v1:v2:vs) (o:os) = let pos = getposition v2
                                 in  uncurry unwindStacks ((Appl pos (Appl pos o v2) v1):vs,os)
unwindStacks (v:vs) (o:os) = ((Appl (getposition v) o v) :vs,os)

getpres::  LamTerm i n -> (Precedence , Associativity)
getpres (Val _ BuildIn{fixity=InFix p a}) = (p,a)
getpres _ = error "no infix"

--TODO correct associative
higer :: [LamTerm i n] -> LamTerm i n -> Bool
higer [] _ = True
higer (x:xs) y= compare (getpres x) (getpres y)
    where compare (p1,a1) (p2,a2) = p1 > p2
