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
    , vName = "plus"
    , arrity = 6
    , fixity = Infix 2 AssoLeft
    , myType = TAppl (TVal TDouble) (TAppl (TVal TDouble ) (TVal TDouble))
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
    , arrity = 7
    , fixity = Infix 3 AssoLeft
    , myType = TAppl (TVal TDouble) (TAppl (TVal TDouble ) (TVal TDouble))
    , evaluator = evalMultiply
    , stack = []
    }

evalMultiply :: State Stack Value
evalMultiply = do
    Prim (MyDouble a) <- pop
    Prim (MyDouble b) <- pop
    return $ Prim $ MyDouble $ a * b

equal :: Value
equal = BuildIn
    { prettyName = "=="
    , vName = "equal"
    , arrity = 2
    , fixity = Infix 4 AssoLeft
    , myType = TAppl (TVal TDouble ) (TAppl (TVal TDouble) (TVal TBool)) -- FIXME
    , evaluator = evalMultiply
    , stack = []
    }

evalEqual :: State Stack Value
evalEqual = do
    a <- pop
    b <- pop
    return $ Prim $ MyBool $ a == b
