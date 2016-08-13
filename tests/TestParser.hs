module TestParser (testParser ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import ArbitraryLambda
import TestSetParseShow
import MakeTerm

import Name
import Lambda
import Operator
import Parser
import PrettyPrint
import Info

testParser :: TestTree
testParser = testGroup "parser"
    [ testParserBasic
    , testParserMath
    , testParserAdvanced
    , testParserLet
    , testParserLetEdge
    , testParserFail
    , testParseAlternatives
    , testParserProperties
    ]

testParserFail :: TestTree
testParserFail = testGroup "should fail"
    [parserFail "(* * 1.0)+ 1.0"]

testParseAlternatives :: TestTree
testParseAlternatives = testGroup "alternative notation"
    [ testCaseParser "\n1.0" $ double 1.0

    , testCaseParser "\\a.\\b.1.0" $ lambda "a" (lambda "b" (double 1.0))

    , testCaseParser "(+) (1.0 2.0)" $
        appl (val plus) (appl (double 1.0) (double 2.0))
    ]

testParserProperties :: TestTree
testParserProperties = testGroup "properties"
    [ testProperty "parse all untype lambda pShow " $
        forAllUnTypedLambda (\ t -> case parseString (pShow t) of
                                        Left _ -> False
                                        _ -> True
                            )
    , testProperty "pShow parse = id " $
        forAllUnTypedLambda $ \ term ->
            let string = pShow term
                parsed = fmap removeInfo (parseString string)
            in counterexample ("\tpShow:        " ++ string ++
                             "\n\tparsed:       " ++ show parsed ++
                             "\n\tpshow parsed: " ++ show (fmap pShow parsed)) $
            case parsed of
                Right t -> t == term
                -- Left Infix {} -> True -- TODO Check , add Lexer
                Left _ -> False
   ]

testSet :: String -> [(String, LamTerm Name () Name)] -> TestTree
testSet name set = testGroup name $ map (uncurry testCaseParser) set

testParserBasic :: TestTree
testParserBasic = testSet "Basix" basic

testParserMath :: TestTree
testParserMath = testSet "Math" math

testParserAdvanced :: TestTree
testParserAdvanced = testSet "Advanced" advanced

testParserLet :: TestTree
testParserLet = testSet "let" letSet

testParserLetEdge :: TestTree
testParserLetEdge = testGroup "let Edge case"
  [ testCaseParser "let leta = 1.0; in leta " (mkLet [("leta", double 1.0)] (var "leta"))
  , testCaseParser "let a = 1.0 in a" (mkLet [("a", double 1.0)] (var "a"))
  ]

testCaseParser :: String -> LamTerm Name () Name -> TestTree
testCaseParser string expect = testCase string $
    let result = fmap removeInfo (parseString string)
        expectM = return expect
    in assertBool ("try to Parse:\n" ++ string ++
                 "\nexpected: " ++ show expect ++
                 "\nbut got : " ++ show result ++
               "\n\npshow expected: " ++ show (fmap pShow expectM) ++
                 "\npshow but got : " ++ show (fmap pShow result ))
             (result == expectM)

parserFail :: String -> TestTree
parserFail string = testCase ("fail at:" ++ string ) $ case parseString string of
    Left _ -> return ()
    Right a -> assertFailure (show a)
