{-# LANGUAGE OverloadedStrings #-}
module TestCodeGen where

-- import qualified Data.ByteString.Char8 as ByteString
-- import LLVM.Module


import Data.DList hiding (map)
import qualified LLVM.AST.Constant as LLVM
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Global as LLVM
import qualified LLVM.AST.Type as LLVM
import LLVM.Analysis

import Test.Tasty
import Test.Tasty.HUnit
import System.Posix.Redirect
import System.Posix.Unistd
import System.IO

import Statments
import Foreign.Ptr
import CodeGen
import Jit
import Type
import Value
import Name
import MakeAnormal

import qualified MakeTerm as L -- FIXME remove should only test codgen not conversion ANormalForm
import ANormalForm

-- TODO remove repetion of code
-- TODO cleanup

foreign import ccall unsafe "putchar"
  putchar ::Int -> IO ()

testCodeGen :: TestTree
testCodeGen = testGroup "CodeGen"
   [ testCase "true" $
      withLLVM (mkModule [codeGen $ val true]) verify
      --TODO use verify for codgen Test
      --     and make separate jit check

   ,  testCase "true" $
      runJit (mkModule [codeGen $ val true]) "main" $ \addres ->
      cast addres TBool >>= (@?= MyBool True)

   , testCase "Fale" $
      runJit (mkModule [codeGen $ val false]) "main" $ \addres ->
      cast addres TBool >>= (@?= MyBool False)

   , testCase "exit" $ do
      hFlush stdout -- TODO wright cleaner solution
      usleep 100000   -- need to flush and sleep otherwise redirectStdout will still capture previous output
      (out,a )<- redirectStdout $ runJit -- use redirectStdout because silently did not caputer c output,
         (mkModule
            [ external "putchar"  [LLVM.Parameter LLVM.i32 (LLVM.Name "exitcode")[]] LLVM.i32
            , LLVM.GlobalDefinition $ LLVM.functionDefaults
              { LLVM.name = "main"
              , LLVM.returnType = LLVM.i32
              , LLVM.basicBlocks = mergBlocks $ toList $ snd $ toBlock (Just " entry") []
                (\[] -> LLVM.LocalReference LLVM.i32 <$> (toStatments $ callFunction
                                        "putchar"
                                        [(LLVM.i32 ,(LLVM.ConstantOperand $ LLVM.Int 32 42))]
                                        LLVM.i32)
                )
              }
            ])
         "main" $ \address ->  do
            fnToInt32 (castPtrToFunPtr $ wordPtrToPtr address)
      (out,a) @?= ("*",42)

   , testCase "42.0" $
      runJit (mkModule [codeGen $ val $ double 42]) "main" $ \addres ->
      cast addres TDouble>>= (@?= MyDouble 42)

   , testCase "4+5" $do
      let modul = mkModule [codeGen $ appl  plus [double 4, double 5]]
      runJit modul  "main" $ \addres ->
       castFn addres TDouble>>= (@?= MyDouble 9)

   , testCase "4+5+6" $do  --TODO remove Lambda and make anormal form
      let anorm = aNormalize $
            (L.plus `L.appl`
                  ((L.plus `L.appl` L.double 4) `L.appl` L.double 5))
            `L.appl` L.double 6
      let modul = mkModule [codeGen anorm]
      runJit modul  "main" $ \addres ->
       castFn addres TDouble>>= (@?= MyDouble 15)

   , testCase "(\\a b-> a + b) 1 2" $ do
      let modul = mkModule [codeGen $
            appl (lambda [Name "a",Name "b"] $ appl plus [bvar 0, bvar 1])
               [double 1 , double 2]]
      runJit modul  "main" $ \addres ->
       castFn addres TDouble>>= (@?= MyDouble 3)
   ]
