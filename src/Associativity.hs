module Associativity where

data Fixity = Prefix | Infix Precedence Associativity
type Precedence = Int


-- TODO make datatype for (Precedence,Associativity) and hide implementation
-- TODO at support for non Associativity
data Associativity
  = AssoRight -- ^ Right Associative.
  | AssoLeft  -- ^ Left Associative.
  deriving Eq
-- | Compare Precedence and returns true if the first argument bind tighter.
--   It expect that the order of its argument's to be the same as the corresponding functions
--   This is so it can take Associativity in to account

higherPrec :: (Precedence, Associativity) -> (Precedence, Associativity) -> Bool
higherPrec (p1, AssoLeft) (p2, _) = p1 >= p2
higherPrec (p1, AssoRight) (p2, _) = p1 > p2

-- | Highest possible Precedence and Associativity.
--
-- Is Right Associative.
highPrec :: (Precedence, Associativity)
highPrec = (100, AssoLeft)

-- |Lowest possible Precedence and Associativity.
--
-- Is Right Associative.
lowPrec :: (Precedence, Associativity)
lowPrec = (0, AssoRight)
