module TestParser (testParser ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ArbitraryQuickcheck ()
import Data.Either

import Opperator
import Lambda
import Vallue
import Parser
import Expresion 
import qualified ExampleLambda as L
testParser :: TestTree
testParser = testGroup "parser"
  [
    testCase "id id id" $
     let ididid = "(\\a.a)(\\b.b)\\c.c"
     in (pShow (right (parseString ididid)) @?= ididid)
  , testCase "+" $
     right (parseString "+") @?= (Val plus)

  , testCase "1 +" $
     right (parseString "1.0 +")
     @?= 
     (Appl (Val plus) (Val (MyDouble 1)))
  , testCase "+ 1" $
     right (parseString "+ 1.0")
     @?= 
     (Appl(Lambda "#"(Appl (Val plus)( Var "#") ))(Val (MyDouble 1)))
  , testCase "1 + 2" $
     right (parseString "1.0 + 2.0")
     @?= 
     (Appl (Appl (Val plus) (Val (MyDouble 1))) (Val (MyDouble 2)))
  , testCase "1 * 2 + 3" $
     right (parseString "1.0 * 2.0 + 3.0")
     @?=
     Appl (Appl (Val plus) 
                (Appl (Appl (Val multiply) (Val (MyDouble ( 1.0)))) (Val (MyDouble ( 2.0)))))
          (Val (MyDouble ( 3.0))) 
  , testCase "1 + 2 * 3" $
     right (parseString "1.0 + 2.0 *3.0")
     @?= 
     (Appl (Appl (Val plus) (Val (MyDouble 1))) 
           (Appl (Appl 
                      (Val multiply )
                      (Val (MyDouble 2 )))
                 (Val (MyDouble 3))))
  , testCase "1 * 2 + 3 * 4 " $
     right (parseString "1.0 * 2.0 + 3.0 * 4.0"  )
     @?=
     Appl (Appl (Val plus) 
                (Appl (Appl (Val multiply) (Val (MyDouble 1.0))) (Val (MyDouble  2.0)))) 
                (Appl (Appl (Val multiply) (Val (MyDouble 3.0))) (Val (MyDouble  4.0)))

  , testCase "*\\a.a" $
     right (parseString "*\\a.a"  )
     @?=
     (Appl (Lambda "#" $ Appl (Val multiply)( Var "#" )) L.id )

  , testCase "(\\a.a)(*)" $
     right (parseString "(\\a.a)(*)"  )
     @?=
     ( Appl  L.id (Val multiply))
  , testCase "(\\a.a)*" $
     right (parseString "(\\a.a)*"  )
     @?=
     ( Appl  (Val multiply) L.id )
  , testProperty "parse pShow arbitrary " $
        \ t -> isRight (parseString (pShow t  ))
  , testProperty "pShow parse = id " $
        \ t -> (right (parseString (pShow t ))) == t
  ]


right :: Show a => Either a b -> b
right (Right b) = b
right (Left a) = error $ show a
