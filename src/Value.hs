module Value where

import Control.Monad.State.Strict
import Type
import Associativity

type Stack = [Value]
-- TODO make every type a build in
-- TODO do whee realy need name and prettyName
-- TODO stack should not be part of value, make new value type closure ?
-- TODO arrity can be calcultated
data Value = Prim Primative
    | BuildIn { prettyName :: String
              , vName :: String
              , arrity :: Int
              , fixity :: Fixity
              , myType :: Type
              , evaluator :: State Stack Value
              , stack :: Stack
              }

data Primative = MyDouble Double
               | MyBool Bool -- make Bool libary data type (sum type)

-- TODO remove if every type is build in
getType :: Value -> Type
getType (Prim p) = getTypePrimative p
getType BuildIn {myType = t} = t

getTypePrimative :: Primative -> Type
getTypePrimative MyDouble {} = TVal TDouble
getTypePrimative MyBool {} = TVal TBool

isInfix :: Value -> Bool
isInfix BuildIn {fixity = Infix {} } = True
isInfix _ = False

pShowVal :: Value -> String
pShowVal (Prim p) = pShowPrimitive p
pShowVal BuildIn {prettyName = n} = n

pShowPrimitive :: Primative -> String
pShowPrimitive (MyDouble d) = show d
pShowPrimitive (MyBool b) = show b

instance Show Value where
    show (Prim p) = show p
    show BuildIn {vName = n, stack = s } = n ++
        if null s
        then ""
        else show s

instance Show Primative where
  show (MyDouble a) = "(MyDouble ( " ++ show a ++ "))"
  show (MyBool b) = show b

instance Eq Value where
  BuildIn {vName = n1 , stack = s1} == BuildIn {vName = n2 , stack = s2} =
    n1 == n2 && s1 == s2
  (Prim p1) == (Prim p2 ) = p1 == p2
  _ == _ = False

instance Eq Primative where
    (==) (MyDouble a) (MyDouble b) = abs (a - b) < 0.0001 -- TODO make more precise
    (==) (MyBool a) (MyBool b) = a == b
    (==) _ _ = False
