module TestBackList where


import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Exception

import BackList

testBacktrack :: TestTree
testBacktrack = testGroup "backtrack"
  [ testGuardFail
  , testGuardFailBacksteps
  , testGuardFail'
  , testGuardFailBacksteps'
  , testGuardSucces
  , testGuardSuccesBacksteps
  , testGuardSucces'
  , testGuardSuccesBacksteps'
  , testLimitBacksteps
  , twoBacksteps
  , stopMzero
  ]

testLimitBacksteps :: TestTree
testLimitBacksteps = assertFail "limitBackSteps" (
   let f y = if y >= 0
       then let result = tryM (map f [0 .. y - 1]) :: BackList Int
                steps = backsteps result
            in if steps > 3
               then error "to many backsteps"
               else result
        else mzero
   in null $ toList $ f =<< try [(0 :: Int) .. ]) "to many backsteps"

assertFail :: String -> a -> String -> TestTree
assertFail nameTest a exception = testCase nameTest $
 do
    result <- Control.Exception.try (Control.Exception.evaluate a)
    case result of
     Right _ -> assertFailure ("test should fail with: " ++ exception)
     Left e -> unless (show (e :: Control.Exception.ErrorCall) == exception) $
            assertFailure ("test faild with: " ++ show e ++
                      "\nexpect fail with: " ++ exception)

testGuardFail :: TestTree
testGuardFail = testCase "guard fail return" $ toList guardFail @?= []

testGuardFailBacksteps :: TestTree
testGuardFailBacksteps = testCase "fail  guard return is one backstep" $
                     backsteps guardFail @?= 2

testGuardFail' :: TestTree
testGuardFail' = testCase "guard fail" $ toList guardFail' @?= []

testGuardFailBacksteps' :: TestTree
testGuardFailBacksteps' = testCase "fail guard  is one backstep" $
                     backsteps guardFail' @?= 2

testGuardSucces :: TestTree
testGuardSucces = testCase "guard succes return" $ toList guardSucces @?= [10]

testGuardSuccesBacksteps :: TestTree
testGuardSuccesBacksteps = testCase "guard succes return is no backstep" $
                     backsteps guardSucces @?= 0

testGuardSucces' :: TestTree
testGuardSucces' = testCase "guard succes" $ toList guardSucces' @?= [()]

testGuardSuccesBacksteps' :: TestTree
testGuardSuccesBacksteps' = testCase "guard succes is no backstep" $
                     backsteps guardSucces' @?= 0

guardFail :: BackList Int
guardFail = do
  x <- try [1 .. 10 :: Int]
  guard (x == 11)
  return x

guardSucces :: BackList Int
guardSucces = do
  x <- try [1 .. 10 :: Int]
  guard (x == 10)
  return x

guardFail' :: BackList ()
guardFail' = do
  x <- try [1 .. 10 :: Int]
  guard (x == 11)

guardSucces' :: BackList ()
guardSucces' = do
  x <- try [1 .. 10 :: Int]
  guard (x == 10)

stopMzero :: TestTree
stopMzero = testCase "stop ad mzero" ( backsteps (do
  i <- try []
  tryM [try [i], mzero]) @?= 1)

twoBacksteps :: TestTree
twoBacksteps = testCase "2 backsteps " ( backsteps (
  tryM [try [], mzero]) @?= 2)
