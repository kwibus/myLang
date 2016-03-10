module Operator where

import Value
import Control.Monad.State.Strict
import MakeType
import Associativity

pop :: State Stack Value
pop = do
    s <- get
    put $ tail s
    return $ head s

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
    Prim (MyDouble a) <- pop
    Prim (MyDouble b) <- pop
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
    Prim (MyDouble a) <- pop
    Prim (MyDouble b) <- pop
    return $ Prim $ MyDouble $ a * b
