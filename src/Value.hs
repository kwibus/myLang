module Value where

import Control.Monad.State.Strict
import Type
import Associativity

type Stack = [Value]
-- TODO make every type a build in
data Value = MyDouble !Double
    | BuildIn { prettyName :: String
              , vName :: String
              , arrity :: Int
              , fixity :: Fixity
              , myType :: Type
              , evaluator :: State Stack Value
              , stack :: Stack
              }

-- TODO remove if every type is build in
getType :: Value -> Type
getType MyDouble {} = TVal TDouble
getType BuildIn {myType = t} = t

isInfix :: Value -> Bool
isInfix BuildIn {fixity = Infix {} } = True
isInfix _ = False

pShowVal :: Value -> String
pShowVal (MyDouble a) = show a
pShowVal BuildIn {prettyName = n} = n

instance Show Value where
    show (MyDouble a) = "(MyDouble ( " ++ show a ++ "))"
    show BuildIn {vName = n, stack = s } = n ++
        if null s
        then ""
        else show s

instance Eq Value where
    (==) (MyDouble a) (MyDouble b) = abs (a - b) < 0.0001
    (==) BuildIn {vName = n1 } BuildIn {vName = n2 } = n1 == n2
    (==)_ _ = False
