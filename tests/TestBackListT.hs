module TestBackListT where

import Control.Monad.Identity
import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck
import Control.Exception
import BackListT

testBackListT :: TestTree
testBackListT = testGroup "backListT"
  [ testCase "1+2" $ generate (backstepsT (tryMT [stepsT 1, stepsT 2])) >>= (@?= 3 )
  , testLazy1
  , testLazy2
  , testGuardFail
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

testLazy1 :: TestTree
testLazy1 = testCase "tryT is lazy" $ generate (backstepsT $ tryT [(1 :: Int) ..]) >>= (@?= 0)

testLazy2 :: TestTree
testLazy2 = testCase "tryTM is lazy" $ generate (backstepsT $ tryMT $ map return [(1 :: Int) ..]) >>= (@?= 0)

testLimitBacksteps :: TestTree
testLimitBacksteps = assertFail "limitBackSteps" (
  let f y = if y >= 0
      then
          let result = tryMT (map f [0 .. y - 1]) :: BackListT Gen Int
          in BackListT $ do
              steps <- backstepsT result
              if steps > 3
              then error "to many backsteps"
              else run result
      else mzero
  in generate ( toListT $ f =<< tryT [(0 :: Int) .. ])) "to many backsteps"

-- TODO move sepport module
assertFail :: Show a => String -> IO a -> String -> TestTree
assertFail nameTest a exception = testCase nameTest $
 do
   eitherResult <- join $ fmap (Control.Exception.try . Control.Exception.evaluate) a
   case eitherResult of
     Right result -> assertFailure $ "test didn't fail" ++
                       "\ntest should fail with: " ++ exception ++
                       "\nbut got: " ++ show result
     Left e -> unless (show (e :: Control.Exception.ErrorCall) == exception) $
            assertFailure ("test faild with: " ++ show e ++
                         "\nexpect fail with: " ++ exception)

testGuardFail :: TestTree
testGuardFail = testCase "guard fail return" $ generate (toListT guardFail) >>= (@?= [])

testGuardFailBacksteps :: TestTree
testGuardFailBacksteps = testCase "fail  guard return is one backstep" $
                     generate (backstepsT guardFail) >>= (@?= 2)

testGuardFail' :: TestTree
testGuardFail' = testCase "guard fail" $ generate (toListT guardFail') >>= (@?= [])

testGuardFailBacksteps' :: TestTree
testGuardFailBacksteps' = testCase "fail guard  is one backstep" $
                     generate (backstepsT guardFail') >>= (@?= 2)

testGuardSucces :: TestTree
testGuardSucces = testCase "guard succes return" $ generate (toListT guardSucces) >>= (@?= [10])

testGuardSuccesBacksteps :: TestTree
testGuardSuccesBacksteps = testCase "guard succes return is no backstep" $
                     generate (backstepsT guardSucces) >>= (@?= 0)

testGuardSucces' :: TestTree
testGuardSucces' = testCase "guard succes" $ generate (toListT guardSucces') >>= (@?= [()])

testGuardSuccesBacksteps' :: TestTree
testGuardSuccesBacksteps' = testCase "guard succes is no backstep" $
                     generate (backstepsT guardSucces') >>= (@?= 0)

guardFail :: BackListT Gen Int
guardFail = do
  x <- tryT [1 .. 10 :: Int]
  guard (x == 11)
  return x

guardSucces :: BackListT Gen Int
guardSucces = do
  x <- tryT [1 .. 10 :: Int]
  guard (x == 10)
  return x

guardFail' :: BackListT Gen ()
guardFail' = do
  x <- tryT [1 .. 10 :: Int]
  guard (x == 11)

guardSucces' :: BackListT Gen ()
guardSucces' = do
  x <- tryT [1 .. 10 :: Int]
  guard (x == 10)

stopMzero :: TestTree
stopMzero = testCase "stop ad mzero" $ do
  result <- generate $ backstepsT (do
        i <- tryT []
        tryMT [tryT [i], mzero])
  result @?= 1

twoBacksteps :: TestTree
twoBacksteps = testCase "2 backsteps " $ do
   result <- generate $ backstepsT $ tryMT [tryT [], mzero]
   result @?= 2
