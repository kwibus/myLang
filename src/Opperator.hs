module Opperator where

import Vallue
import Control.Monad.State.Strict

push :: Vallue -> State Stack ()
push v = modify (v :)
data Type = TDouble | TBool

pop :: State Stack Vallue
pop = do
    s <- get
    put $ tail s
    return $ head s


plus :: Vallue
plus = BuildIn { name = "plus"
               , arrity = 2
               , evaluator = evalplus
               , stack = []
               }

evalplus :: State Stack Vallue
evalplus = do
    MyDouble a <- pop
    MyDouble b <- pop
    return $ MyDouble $ a + b

multiply :: Vallue
multiply = BuildIn { name = "multiply"
                   , arrity = 2
                   , evaluator = evalMultiply
                   , stack = []
                   }

evalMultiply :: State Stack Vallue
evalMultiply = do
    MyDouble a <- pop
    MyDouble b <- pop
    return $ MyDouble $ a * b
