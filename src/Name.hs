module Name where

import Control.Monad

-- it is a newtype not Type String so it can be a instant of typeclass
-- needed for test/ArbiRef

data Name = Name String
          | DummyBegin
          | DummyEnd
          deriving (Eq, Show, Ord)

toString :: Name -> String
toString (Name str) = str
toString DummyBegin= "#"
toString DummyEnd= "##"

prettyPrint :: Name -> String
prettyPrint (Name str) = str
prettyPrint _ = ""

fromString :: String -> Name
fromString str = case str of
    "" -> error "can't have empty name"
    "#"->DummyBegin
    "##"-> DummyEnd
    string -> Name string

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']
