module Value where

import Control.Monad.State.Strict
import Type (Type,dropTypeArg)
import MakeType
import Associativity

type Stack = [Value]

-- TODO make every type a build in
data Value = Prim Primative
           | Func BuildIn
           deriving (Show,Eq)

data BuildIn = BuildIn
              { prettyName :: String -- TODO why 2 names
              , vName :: String
              , arrity :: Int --TODO mispeld
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
getType (Func BuildIn {myType = t}) = t

getTypePrimative :: Primative -> Type
getTypePrimative MyDouble {} = tDouble
getTypePrimative MyBool {} = tBool

isInfix :: Value -> Bool
isInfix (Func BuildIn {fixity = Infix {}}) = True
isInfix _ = False

pShowVal :: Value -> String
pShowVal (Prim p) = pShowPrimitive p
pShowVal (Func BuildIn {prettyName = n}) = n

pShowPrimitive :: Primative -> String
pShowPrimitive (MyDouble d) = show d
pShowPrimitive (MyBool b) = show b

instance Show BuildIn where
    show BuildIn {vName = n, stack = s } = n ++
        if null s
        then ""
        else show s

instance Show Primative where
  show (MyDouble a) = "(MyDouble ( " ++ show a ++ "))"
  show (MyBool b) = show b


instance Eq BuildIn where
  BuildIn {vName = n1 , stack = s1} == BuildIn {vName = n2 , stack = s2} =
    n1 == n2 && s1 == s2

instance Eq Primative where
    (==) (MyDouble a) (MyDouble b) = abs (a - b) < 0.0001 -- TODO make more precise
    (==) (MyBool a) (MyBool b) = a == b
    (==) _ _ = False

--TODO Fix
-- | applys a build in function to one argument
--  It crashes if first argument is not a function (It only export to include int test)
applyValue :: Value -- ^ build in function
           -> Value -- ^ argument
           -> Value -- ^ result
applyValue ( Func BuildIn {arrity = 1, evaluator = e, stack = s }) v = evalState e (v : s )
applyValue (Func v1@BuildIn {arrity = n, stack = s, myType = t }) v2 =
    Func $ v1 {arrity = n - 1 , stack = v2 : s, myType = dropTypeArg t}
applyValue v _ = error $ show v ++ " apply value"
