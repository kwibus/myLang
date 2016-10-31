module TestFail where


import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception
import Control.Monad

assertFailWith :: String -> a -> String -> TestTree
assertFailWith nameTest a exception = testCase nameTest $
 do
    result <- try (evaluate a)
    case result of
     Right _ -> assertFailure ("test should fail with: " ++ exception)
     Left (ErrorCallWithLocation e _) -> unless (e  == exception) $
            assertFailure ("test   failed with: " ++ show e ++
                         "\nexpect failed with: " ++ show exception)
