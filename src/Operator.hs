module Operator where

import Value
import Control.Monad.State.Strict
import Type
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
    , name = "plus"
    , arrity = 2
    , fixity = Infix 2 AssoLeft
    , myType = toPoly $ TAppl (TVal TDouble) (TAppl (TVal TDouble ) (TVal TDouble))
    , evaluator = evalplus
    , stack = []
    }

evalplus :: State Stack Value
evalplus = do
    MyDouble a <- pop
    MyDouble b <- pop
    return $ MyDouble $ a + b

multiply :: Value
multiply = BuildIn
    { prettyName = "*"
    , name = "multiply"
    , arrity = 2
    , fixity = Infix 3 AssoLeft
    , myType = toPoly $ TAppl (TVal TDouble) (TAppl (TVal TDouble ) (TVal TDouble))
    , evaluator = evalMultiply
    , stack = []
    }

evalMultiply :: State Stack Value
evalMultiply = do
    MyDouble a <- pop
    MyDouble b <- pop
    return $ MyDouble $ a * b
