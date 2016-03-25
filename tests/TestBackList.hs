module TestBackList where

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Exception

import BackList

testBackList  :: TestTree
testBackList = testGroup "BackList" [testBacktrack , testBackSteps]

testBackSteps :: TestTree
testBackSteps = testGroup "backsteps"
    [ testCase "1 backsteps " $
        toList ( tryM
          [ tryM
            [tryM
              [ StepBack 1
              , return (3::Int)
              ]
            ,return 2
            ]
          , return 1
          ])
         @?= [2,1]
    , testCase "2 backsteps " $
        toList ( tryM
          [ tryM
            [tryM
              [ StepBack 2
              , return (3::Int)
              ]
            , return 2
            ]
          , return 1
          ])
         @?= [1]

    , testCase "many backsteps " $
        toList ( tryM
          [ tryM
            [tryM
              [ StepBack 10
              , return (3::Int)
              ]
            , return 2
            ]
          , return 1
          ])
         @?= []

    , testCase "save [1,2,3]" $
        ( save $ try [1..(3::Int)])
        @?= Node [Leaf 1 Top] (Node [Leaf 2 Top,Leaf 3 Top] Top)
    , testCase "do many backsteps" $
        ( do
           i <- try [1..(3::Int)]
           j <- try [1..3]
           (i*j*)<$> try [1..3]
           -- if odd (i + j + k) then  StepBack 1 else return (i,j,k)
           )
        @?=return 1

    , testCase "do many backsteps" $
        ( do
           i <- save $ try [1..(3::Int)]
           j <- try [1..(3::Int)]
           (\k->(i,j,k))<$> try [1..(3::Int)]
           -- if odd (i + j + k) then  StepBack 1 else return (i,j,k)
           )
        @?=return (1,1,1)
    ]

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
  , nestedMzero
  , stopMzero
  ]

testLimitBacksteps :: TestTree
testLimitBacksteps = assertFail "limitBackSteps" (
   let f y = if y >= 0
       then let result = tryM (map f [0 .. y - 1]) :: BackList Int
                steps = failures result
            in if steps > 3
               then error "to many backsteps"
               else result
        else mzero
   in null $ toList $ f =<< try [(0 :: Int), 1,2,3,4,5,undefined ]) "to many backsteps"

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
testGuardFailBacksteps = testCase "fail guard return is one backstep" $
                     failures guardFail @?= 10

testGuardFail' :: TestTree
testGuardFail' = testCase "guard fail" $ toList guardFail' @?= []

testGuardFailBacksteps' :: TestTree
testGuardFailBacksteps' = testCase "fail guard  is one backstep" $
                     failures guardFail' @?= 10

testGuardSucces :: TestTree
testGuardSucces = testCase "guard succes return" $ toList guardSucces @?= [10]

testGuardSuccesBacksteps :: TestTree
testGuardSuccesBacksteps = testCase "guard succes return is no backstep" $
                     failures guardSucces @?= 0

testGuardSucces' :: TestTree
testGuardSucces' = testCase "guard succes" $ toList guardSucces' @?= [()]

testGuardSuccesBacksteps' :: TestTree
testGuardSuccesBacksteps' = testCase "guard succes is no backstep" $
                     failures guardSucces' @?= 0

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
stopMzero = testCase "stop ad mzero" ( failures(do
  i <- try []
  tryM [try [i], mzero]) @?= 1)

nestedMzero :: TestTree
nestedMzero = testCase "nested mzero " ( failures(
  tryM [try [], mzero]) @?= 2)
