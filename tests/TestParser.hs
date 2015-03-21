module TestParser (testParser ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ArbitraryQuickcheck ()
import Data.Either

import Opperator
import Parser
import Expresion 
import MakeTerm
import qualified ExampleLambda as L
testParser :: TestTree
testParser = testGroup "parser"
  [
    testCase "id id id" $
     let ididid = "(\\a.a)(\\b.b)\\c.c"
     in (pShow (right (parseString ididid)) @?= ididid)
  , testCase "+" $
     right (parseString "+") @?= (val plus)

  , testCase "1 +" $
     right (parseString "1.0 +")
     @?= 
     (appl (val plus) (double 1))
  , testCase "+ 1" $
     right (parseString "+ 1.0")
     @?= 
     (appl(lambda "#"(appl (val plus)( var "#") ))(double 1))
  , testCase "1 + 2" $
     right (parseString "1.0 + 2.0")
     @?= 
     appl (appl (val plus) (double 1)) (double 2)
  , testCase "1 * 2 + 3" $
     right (parseString "1.0 * 2.0 + 3.0")
     @?=
     appl (appl (val plus)
                (appl (appl (val multiply) (double 1.0)) (double 2.0)))
          (double  3.0) 
  , testCase "1 + 2 * 3" $
     right (parseString "1.0 + 2.0 *3.0")
     @?= 
     (appl (appl (val plus) (double 1)) 
           (appl (appl 
                      (val multiply )
                      (double 2 ))
                 (double 3)))
  , testCase "1 * 2 + 3 * 4 " $
     right (parseString "1.0 * 2.0 + 3.0 * 4.0"  )
     @?=
     appl (appl (val plus)
                (appl (appl (val multiply) (double 1.0)) (double  2.0)))
                (appl (appl (val multiply) (double 3.0)) (double  4.0))

  , testCase "*\\a.a" $
     right (parseString "*\\a.a"  )
     @?=
     (appl (lambda "#" $ appl (val multiply)( var "#" )) L.id )

  , testCase "(\\a.a)(*)" $
     right (parseString "(\\a.a)(*)"  )
     @?=
     ( appl  L.id (val multiply))
  , testCase "(\\a.a)*" $
     right (parseString "(\\a.a)*"  )
     @?=
     ( appl  (val multiply) L.id )
  , testProperty "parse pShow arbitrary " $
        \ t -> isRight (parseString (pShow t  ))
  , testProperty "pShow parse = id " $
        \ t -> (right (parseString (pShow t ))) == t
  ]


right :: Show a => Either a b -> b
right (Right b) = b
right (Left a) = error $ show a
