module TestPrettyPrintLambda (testPrettyPrintLambda) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Arrow

import ArbitraryLambda

import Info
import PrettyPrint
import Parser
import Lambda
import TestSetParseShow

testPrettyPrintLambda :: TestTree
testPrettyPrintLambda = testGroup "pShow"
  [ testPShowBasic
  , testPShowLet
  , testPShowMath
  , testPShowAdvanced
  , testNoRedundantParens
  ]

testPShowMath :: TestTree
testPShowMath = testGroup "Math" $ map testPShowExample math

testPShowBasic :: TestTree
testPShowBasic = testGroup "Basix" $ map testPShowExample basic

testPShowLet :: TestTree
testPShowLet = testGroup "Let" $ map testPShowExample letSet

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
          newpost -> (pre, post) : map (first ((pre ++ "(") ++)) (findStarts (tail newpost))
    deleteParens :: (String, String) -> String
    deleteParens (_, []) = []
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

