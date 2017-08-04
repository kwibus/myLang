module Value where

import Control.Monad.State.Strict
import Type (Type,dropTypeArg)
import MakeType
import Associativity

type Stack = [Value]
-- TODO make every type a build in
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
getTypePrimative MyDouble {} = tDouble
getTypePrimative MyBool {} = tBool

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


-- | applys a build in function to one argument
--  It crashes if first argument is not a function (It only export to include int test)
applyValue :: Value -- ^ build in function
           -> Value -- ^ argument
           -> Value -- ^ result
applyValue BuildIn {arrity = 1, evaluator = e, stack = s } v = evalState e (v : s )
applyValue v1@BuildIn {arrity = n, stack = s, myType = t } v2 =
    v1 {arrity = n - 1 , stack = v2 : s, myType = dropTypeArg t}
applyValue v _ = error $ show v ++ " apply value"
