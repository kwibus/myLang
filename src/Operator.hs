module Operator where

import Value
import Control.Monad.State.Strict
import MakeType
import Associativity

-- TODO maybe refactor buildin is defined from here
-- TODO make builder function for buildin operators (arrity 2)

pop :: (Value -> a )-> State Stack a
pop f = do
    s <- get
    put $ tail s
    return $ f $ head s

popDouble :: State Stack Double
popDouble = pop (\v -> case v of
    Prim (MyDouble n) -> n
    _ -> error $ "expeceted doubel on the stack. but got " ++ show v)

operators :: [BuildIn]
operators = [plus, multiply]

plus :: BuildIn
plus = BuildIn
    { prettyName = "+"
    , vName = "plus"
    , arrity = 2
    , fixity = Infix 2 AssoLeft
    , myType = tDouble ~> tDouble ~> tDouble
    , evaluator = evalplus
    , stack = []
    }

evalplus :: State Stack Value
evalplus = do
    a <- popDouble
    b <- popDouble
    return $ Prim $ MyDouble $ a + b

multiply :: BuildIn
multiply = BuildIn
    { prettyName = "*"
    , vName = "multiply"
    , arrity = 2
    , fixity = Infix 3 AssoLeft
    , myType = tDouble ~> tDouble ~> tDouble
    , evaluator = evalMultiply
    , stack = []
    }

evalMultiply :: State Stack Value
evalMultiply = do
    a <- popDouble
    b <- popDouble
    return $ Prim $ MyDouble $ a * b
