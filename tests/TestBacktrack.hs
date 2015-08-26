module TestBacktrack where

import Test.Tasty
import Control.Monad
import Test.Tasty.HUnit
import StateTransMany
import qualified Control.Exception

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
  ]

testLimitBacksteps :: TestTree
testLimitBacksteps = assertFail "limitBackSteps" (
   let f y = if y >= 0
       then let result = tryM (map f [0 .. y - 1]) :: StateTransMany () Int
            in do
                 steps <- backsteps result
                 if steps > 3
                 then error "ads"
                 else result
        else mzero
   in eval (f =<< try [(0 :: Int) .. ]) () @?= []) "ads"

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
testGuardFail = testCase "guard fail return" $ eval guardFail () @?= []

testGuardFailBacksteps :: TestTree
testGuardFailBacksteps = testCase "fail  guard return is one backstep" $
                     eval (backsteps guardFail ) () @?= [1]

testGuardFail' :: TestTree
testGuardFail' = testCase "guard fail" $ eval guardFail' () @?= []

testGuardFailBacksteps' :: TestTree
testGuardFailBacksteps' = testCase "fail guard  is one backstep" $
                     eval (backsteps guardFail' ) () @?= [1]

testGuardSucces :: TestTree
testGuardSucces = testCase "guard succes return" $ eval guardSucces () @?= [10]

testGuardSuccesBacksteps :: TestTree
testGuardSuccesBacksteps = testCase "guard succes return is no backstep" $
                     eval (backsteps guardSucces ) () @?= [0]

testGuardSucces' :: TestTree
testGuardSucces' = testCase "guard succes" $ eval guardSucces' () @?= [()]

testGuardSuccesBacksteps' :: TestTree
testGuardSuccesBacksteps' = testCase "guard succes is no backstep" $
                     eval (backsteps guardSucces' ) () @?= [0]
guardFail :: StateTransMany () Int
guardFail = do
  x <- try [1 .. 10 :: Int]
  guard (x == 11)
  return x

guardSucces :: StateTransMany () Int
guardSucces = do
  x <- try [1 .. 10 :: Int]
  guard (x == 10)
  return x

guardFail' :: StateTransMany () ()
guardFail' = do
  x <- try [1 .. 10 :: Int]
  guard (x == 11)

guardSucces' :: StateTransMany () ()
guardSucces' = do
  x <- try [1 .. 10 :: Int]
  guard (x == 10)

-- test3 :: ([()], Int)
-- test3 = eval $ do
--   i <- try []
--   try [i] <|> mempty
