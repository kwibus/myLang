
{-# LANGUAGE OverloadedStrings #-}
module TestExternal where

import Foreign.Ptr
import Test.Tasty
import Test.Tasty.HUnit

import LLVM.AST hiding (address,args,Name,resultType)
import LLVM.AST.Global

import qualified LLVM.AST.Name as LLVM
import qualified LLVM.AST.Global as LLVM
import LLVM.AST.Constant as C hiding (address)
import LLVM.AST.Type as LLVM

import TestUtils
import CodeGen
import Jit
import ExternalFunctions

testExternal :: TestTree
testExternal =   testGroup "external"
   [ testCase "putchar" $ do
      (out,a )<- capture $ runJit
         (mkModule
            [ external "putchar"  [i32 ] i32
            , callWith "main" "putchar" [ Int 32 42] LLVM.i32
            ]) "main" $ \address ->  fnToInt32 (castPtrToFunPtr $ wordPtrToPtr address)
      (out,a) @?= ("*",42)

   , testCase "puts" $ do
      (out,a) <- capture $ -- runJit --TODO
                           runMCJIT
         (mkModule
            [ puts
            , GlobalDefinition
                  (globalVariableDefaults
                    { LLVM.name = LLVM.Name "message"
                    , LLVM.AST.Global.type'= ArrayType 12 LLVM.i8
                    , initializer = Just $ string "hallo world"
                    })

            , callWith "main" "puts"
              [ C.GetElementPtr
                  True
                  (GlobalReference
                     (LLVM.ptr $ ArrayType 12  LLVM.i8 )
                     (LLVM.Name "message"))
                  [Int 32 0 , Int 32 0 ]
              ]
              LLVM.i32
            ])
              "main" $ \address ->
                -- fnToInt32 (castPtrToFunPtr $ wordPtrToPtr address)
                fnToInt32 (castFunPtr address)
      (out,a) @?= ("hallo world\n",12)

   , testCase "write" $ do
      (out,a) <- capture $ -- runJit
                           runMCJIT
         (mkModule
            [ GlobalDefinition
                  (globalVariableDefaults
                    { LLVM.name = LLVM.Name "message"
                    , LLVM.AST.Global.type'= ArrayType 12 LLVM.i8
                    , initializer = Just $ string "hallo world"
                    })

            , write
            , callWith "main" "write"
              [ Int 32 1
              , C.GetElementPtr
                  True
                  (GlobalReference
                     (LLVM.ptr $ ArrayType 12  LLVM.i8 )
                     (LLVM.Name "message"))
                  [ Int 32 0, Int 32 0 ]
              , Int 64 11]
              LLVM.i64
            ])
              "main" $ \address -> --fnToInt64 (castPtrToFunPtr $ wordPtrToPtr address)
                fnToInt64 (castFunPtr address)

      (out,a) @?= ("hallo world",11)

   , testCase "perror" $ do
      (out,a )<- capture $ runJit
                           -- runMCJIT
         (mkModule
            [ perror
            , callWith "main" "perror" [Null $ LLVM.ptr LLVM.i8] LLVM.void
            ]) "main" $ \_->  return ()
      (out,a) @?= ("",())

   , testCase "exit" $ do
      (out,a )<- capture $ runJit
                           -- runMCJIT
         (mkModule
            [ exit
            , callWith "main" "exit" [Int 32 0] LLVM.void
            ]) "main" $ \_->  return ()
      (out,a) @?= ("",())
   ]
