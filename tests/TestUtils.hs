{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module TestUtils where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Char

import Data.ByteString (ByteString)
import System.Posix.Redirect
import Control.Concurrent

capture :: IO a -> IO (ByteString,a)
capture f = do

   threadDelay 100000
   redirectStdout  f -- use redirectStdout because silently did not caputer c output,

removeNewLines :: String -> String
removeNewLines str = concatMap (dropWhile isSpace) $ lines str

amplify :: Show a => String -> (a -> Assertion) -> [a] -> TestTree
amplify discription f cases= testCaseSteps discription $ \_->
   mapM_ f cases
