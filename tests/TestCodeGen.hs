{-# LANGUAGE OverloadedStrings #-}
module TestCodeGen where

import LLVM.Analysis

import Test.Tasty
import Test.Tasty.HUnit

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

testCodeGen :: TestTree
testCodeGen =  testGroup "CodeGen"
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

   , testCase "42.0" $
      runJit (mkModule [codeGen $ val $ double 42]) "main" $ \addres ->
      cast addres TDouble>>= (@?= MyDouble 42)

   , testCase "4+5" $ do
      let modul = mkModule [codeGen $ appl  plus [double 4, double 5]]
      runJit modul  "main" $ \addres ->
        castFn addres TDouble>>= (@?= MyDouble 9)

   , testCase "4+5+6" $ do  --TODO remove Lambda and make anormal form
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
