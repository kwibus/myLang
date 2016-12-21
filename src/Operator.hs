module Operator where

import Value
import Control.Monad.State.Strict
import MakeType
import Associativity

pop :: (Value -> a )-> State Stack a
pop f = do
    s <- get
    put $ tail s
    return $ f $ head s

popDouble :: State Stack Double
popDouble = pop (\v -> case v of
    Prim (MyDouble n) -> n
    _ -> error $ "expeceted doubel on the stack. but got" ++ show v)

operators :: [Value]
operators = [plus, multiply]

plus :: Value
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

multiply :: Value
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
