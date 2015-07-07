module TestParser (testParser ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ArbitraryQuickcheck

import ParserType
import Names
import Lambda
import Opperator
import Parser
import Expresion
import MakeTerm
import Info
import qualified ExampleLambda as L

testParser :: TestTree
testParser = testGroup "parser"
  [ testCaseParser "(\\a.a)(\\b.b)\\c.c" $ appl (appl (L.id "a") (L.id "b")) (L.id "c")
  , testCaseParser "+" $ val plus
  , testCaseParser "1.0 +" $ appl (val plus) (double 1)
  , testCaseParser "+ 1.0" $ appl (lambda "#" (appl (val plus) ( var "#") )) (double 1)
  , testCaseParser "1.0 + 2.0" $ appl (appl (val plus) (double 1)) (double 2)
  , testCaseParser "1.0 * 2.0 + 3.0" $
          appl (appl (val plus)
                     (appl (appl (val multiply) (double 1.0)) (double 2.0)))
                     (double 3.0)
  , testCaseParser "1.0 + 2.0 * 3.0" $
         appl (appl (val plus)
               (double 1))
               (appl (appl (val multiply )
                          (double 2 ))
                          (double 3))

  , testCaseParser "1.0 + 2.0 + 3.0" $
      appl (appl (val plus)
                     (appl (appl (val plus) (double 1.0)) (double 2.0)))
                     (double 3.0)

  , testCaseParser "1.0 * 2.0 + 3.0 * 4.0 " $
      appl (appl (val plus)
                (appl (appl (val multiply) (double 1.0)) (double 2.0)))
                (appl (appl (val multiply) (double 3.0)) (double 4.0))

  , testCaseParser "1.0 + 2.0 * 3.0 + 4.0 " $
      appl (appl (val plus)
                 (appl (appl (val plus)
                             (double 1.0))
                             (appl (appl (val multiply)
                                         (double 2.0))
                                         (double 3.0))))
                 (double 4.0)

  , testCaseParser "*\\a.a" $ appl (lambda "#" $ appl (val multiply) ( var "#" )) (L.id "a")
  , testCaseParser "(\\a.a)(*)" $ appl (L.id "a") (val multiply)
  , testCaseParser "(\\a.a)*" $ appl (val multiply) (L.id "a")
  , testParserFail "(* * 1.0)+ 1.0"
  , testProperty "parse all untype lambda pShow " $
        forAllUnTypedLambda (\ t -> case parseString (pShow t) of
                                        Left (Parsec {}) -> False
                                        _ -> True
                                    )
  , testProperty "pShow parse = id " $
        forAllUnTypedLambda $ \ term -> case fmap removeInfo (parseString (pShow term )) of
            Right t -> t == t
            Left (Infix {}) -> True
            Left (Parsec {}) -> False
   ]

testCaseParser :: String -> LamTerm () Name -> TestTree
testCaseParser string result = testCase string $
     fmap removeInfo (parseString string)
     @?=
     return result

testParserFail :: String -> TestTree
testParserFail string = testCase ("fail at:" ++ string ) $ case parseString string of
    Left _ -> return ()
    Right a -> assertFailure (show a)
