module TestPrettyPrintLambda (testPrettyPrintLambda) where

import Debug.Trace
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Either
import Control.Arrow

import ArbitraryLambda

import Info
import PrettyPrint
import Parser
import Lambda
import Name
import TestSetParseShow

-- Todo consider rename TestExpresion  or TestPrintExpresion

testPrettyPrintLambda :: TestTree
testPrettyPrintLambda = testGroup "pShow" [testPShowBasic, testPShowMath, testPShowAdvanced,testNoRedundantParens]

testPShowMath :: TestTree
testPShowMath = testGroup "Math" $ map testPShowExample math

testPShowBasic :: TestTree
testPShowBasic = testGroup "Basix" $ map testPShowExample basic

testPShowAdvanced :: TestTree
testPShowAdvanced = testGroup "Advanced" $ map testPShowExample advanced

testPShowExample :: (String, LamTerm () Name) -> TestTree
testPShowExample (expected, inputTerm) = testCase expected $ pShow inputTerm @?= expected

testNoRedundantParens :: TestTree
testNoRedundantParens = testProperty "no redundant parenthesis" $ forAllUnTypedLambda ( \ term ->
    let testString = pShow term
        mutated = removeOneParensPair testString
        expected = removeInfo <$> parseString testString
    in not (null mutated) ==> counterexample
      ("generated a string with redudant parenthesis:\n " ++ testString) $
      not $ any (\ mutation -> fmap removeInfo (parseString mutation) == expected) mutated)

removeOneParensPair :: String -> [String]
removeOneParensPair = map deleteParens . findStarts
  where
    findStarts :: String -> [(String , String)]
    findStarts str = let (pre, post) = break (== '(') str
      in case post of
          [] -> []
          post -> (pre, post) : map (first ((pre ++ "(") ++)) (findStarts (tail post))
    deleteParens :: (String, String) -> String
    deleteParens (pre, []) = []
    deleteParens (pre, '(' : post) = pre ++ scan 0 [] post
    deleteParens _ = error "this should never happen"
    scan :: Int -> String -> String -> String
    scan _ _ [] = error "missing )"
    scan i reversePre (s : xs)
      | i < 0 = error "missing ( "
      | s == '(' = scan (i + 1) (s : reversePre) xs
      | s == ')' && i == 0 = reverse reversePre ++ xs
      | s == ')' = scan (i - 1) (s : reversePre) xs
      | otherwise = scan i (s : reversePre) xs
