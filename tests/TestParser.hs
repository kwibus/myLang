module TestParser (testParser ) where


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ArbitraryQuickcheck ()

import Parser
import Lambda

testParser :: TestTree
testParser = testGroup "parser"
  [
    testCase "id id id" $
     let ididid = "(\\a.a)(\\b.b)\\c.c"
     in (pShow (right (parseString ididid)) @?= ididid)
  , testProperty "parse pShow arbitrary " $
        \ t -> isRight (parseString (pShow t  ))
  , testProperty "pShow parse = id " $
        \ t -> (right (parseString (pShow t ))) == t
  ]

isRight :: Show a => Either a b -> Bool
isRight (Right _) = True
isRight (Left a) = error $ show a


right :: Show a => Either a b -> b
right (Right b) = b
right (Left a) = error $ show a
