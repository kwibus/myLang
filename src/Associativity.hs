module Associativity where

data Fixity = Prefix | Infix Precedence Associativity
type Precedence = Int
data Associativity = AssoRight | AssoLeft

higherPrec :: (Precedence, Associativity) -> (Precedence, Associativity) -> Bool
higherPrec (p1, AssoLeft) (p2, _) = p1 >= p2
higherPrec (p1, AssoRight) (p2, _) = p1 > p2

lowerPrec :: (Precedence, Associativity) -> (Precedence, Associativity) -> Bool
lowerPrec p1 p2 = not $ higherPrec p1 p2
