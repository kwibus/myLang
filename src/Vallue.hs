module Vallue where

import Control.Monad.State.Strict
import Type
import Lambda
type Stack = [Vallue]
-- TODO make every type a build in
data Vallue = MyDouble Double
    | BuildIn { prettyName :: String
              , name :: String
              , arrity :: Int
              , isinfix :: Bool
              , myType :: Type
              , evaluator :: State Stack Vallue
              , stack :: Stack
              }
-- TODO remove if every type is build in
vtype :: Vallue -> Type
vtype MyDouble {} = Val TDouble
vtype BuildIn {myType = t} = t

isinfixVallue :: Vallue -> Bool
isinfixVallue v@BuildIn {} = isinfix v
isinfixVallue _ = False

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
