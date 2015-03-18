module Vallue where

import Control.Monad.State.Strict
import Type
import Data.Coerce
import Enviroment

data Fixity = PreFix | InFix Precedence Associativity
type Precedence = Int
data Associativity = AssoRight | AssoLeft

type Stack = [Vallue]
-- TODO make every type a build in
data Vallue = MyDouble Double
    | BuildIn { prettyName :: String
              , name :: String
              , arrity :: Int
              , fixity :: Fixity
              , myType :: Type Bound
              , evaluator :: State Stack Vallue
              , stack :: Stack
              }
-- TODO remove if every type is build in
btype :: Vallue -> Type Bound
btype MyDouble {} = TVal TDouble
btype BuildIn {myType = t} = t

ftype :: Vallue -> Type Free
ftype = coerce . btype

isInfixVallue :: Vallue -> Bool
isInfixVallue BuildIn {fixity = InFix {} } = True
isInfixVallue _ = False

pShowVal :: Vallue -> String
pShowVal (MyDouble a) = show a
pShowVal BuildIn {prettyName = n} = n

instance Show Vallue where
    show (MyDouble a) = "(MyDouble ( " ++ show a ++ "))"
    show BuildIn {name = n} = n

instance Eq Vallue where
    (==) (MyDouble a) (MyDouble b) = abs (a - b) < 0.0001
    (==) (BuildIn {name = n1 }) (BuildIn {name = n2 }) = n1 == n2
    (==) _ _ = False
