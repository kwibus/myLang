module TestParser (testParser ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ArbitraryQuickcheck ()
import Data.Either

import Names
import Lambda
import Opperator
import Parser
import Expresion
import MakeTerm
import Info
import qualified ExampleLambda as L

import Text.Parsec.Error
instance Eq ParseError where
   (==) a b = show a == show b

testParser :: TestTree
testParser = testGroup "parser"
  [
    testCase "id id id" $
     let ididid = "(\\a.a)(\\b.b)\\c.c"
     in (pShow (right (parseString ididid)) @?= ididid)
  , testCase "+" $
     fmap removeInfo (parseString "+") @?= return (val plus)
  , testCase "1 +" $
     fmap removeInfo (parseString "1.0 +")
     @?=
     return (appl (val plus) (double 1))
  , testCase "+ 1" $
     fmap removeInfo (parseString "+ 1.0")
     @?=
     return (appl(lambda "#"(appl (val plus)( var "#") ))(double 1))
  , testCase "1 + 2" $
     fmap removeInfo (parseString "1.0 + 2.0")
     @?=
     return (appl (appl (val plus) (double 1)) (double 2))
  , testCase "1 * 2 + 3" $
     fmap removeInfo (parseString "1.0 * 2.0 + 3.0")
     @?=
     return( appl (appl (val plus)
                (appl (appl (val multiply) (double 1.0)) (double 2.0)))
          (double  3.0))
  , testCase "1 + 2 * 3" $
     fmap removeInfo(parseString "1.0 + 2.0 *3.0")
     @?=
     return (appl (appl (val plus) (double 1))
           (appl (appl
                      (val multiply )
                      (double 2 ))
                 (double 3)))
  , testCase "1 * 2 + 3 * 4 " $
     fmap removeInfo (parseString "1.0 * 2.0 + 3.0 * 4.0"  )
     @?=
     return ( appl (appl (val plus)
                (appl (appl (val multiply) (double 1.0)) (double  2.0)))
                (appl (appl (val multiply) (double 3.0)) (double  4.0)))

  , testCase "*\\a.a" $
     fmap removeInfo (parseString "*\\a.a"  )
     @?=
     return (appl (lambda "#" $ appl (val multiply)( var "#" )) L.id )

  , testCase "(\\a.a)(*)" $
     fmap removeInfo(parseString "(\\a.a)(*)"  )
     @?=
     return ( appl  L.id (val multiply))
  , testCase "(\\a.a)*" $
     fmap removeInfo (parseString "(\\a.a)*"  )
     @?=
     return( appl  (val multiply) L.id )
  , testProperty "parse pShow arbitrary " $
        \ t -> isRight (parseString (pShow (t:: LamTerm () Name  )))
  , testProperty "pShow parse = id " $
        \ t -> (fmap removeInfo (parseString (pShow (t:: LamTerm () Name )))) == return t
  ]


right :: Show a => Either a b -> b
right (Right b) = b
right (Left a) = error $ show a
