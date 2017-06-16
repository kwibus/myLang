module TestUtils where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Char

removeNewLines :: String -> String
removeNewLines str = concatMap (dropWhile isSpace) $ lines str

amplify :: Show a => String -> (a -> Assertion) -> [a] -> TestTree
amplify discription f cases= testCaseSteps discription $ \_->
   mapM_ f cases
