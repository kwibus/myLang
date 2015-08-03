module Associativity where

data Fixity = PreFix | InFix Precedence Associativity
type Precedence = Int
data Associativity = AssoRight | AssoLeft

higerPres :: (Precedence, Associativity) -> (Precedence, Associativity) -> Bool
higerPres (p1, AssoLeft) (p2, _) = p1 >= p2
higerPres (p1, AssoRight) (p2, _) = p1 > p2

lowerPres :: (Precedence, Associativity) -> (Precedence, Associativity) -> Bool
lowerPres p1 p2 = not $ higerPres p1 p2
