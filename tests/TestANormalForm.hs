module TestANormalForm where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Either

import MakeAnormal as A
import MakeTerm as L
import ArbitraryLambda
import Properties

import ANormalForm
import qualified Operator as B
import TypeCheck
import FreeEnvironment
import qualified Type --TODO maybe report Free
import BruijnTerm as Lam
import Name

testANormalForm :: TestTree
testANormalForm = testGroup "Anormalform"
   [ testCase "+" $
      aNormalize L.plus
      @?= A.val ( A.lambda [DummyBegin, DummyEnd] $ A.appl (Instruc B.plus) [A.bvar 0, A.bvar 1])

   , testCase "1+" $
      aNormalize (L.appl L.plus (L.double 1))
      @?= A.val ( A.lambda [DummyEnd] $ A.appl (Instruc B.plus) [A.double 1, A.bvar 0])

   , testCase "\\a.a true (+)" $
      aNormalize ( L.lambda "a" $ L.appl (L.appl (L.bvar 0) L.true) L.plus)
      @?= A.val ( A.lambda [Name "a"] $ A.appl (A.bvar 0) [A.true, A.plus])

   , testCase "\\a.(+) (a true)" $
      aNormalize ( L.lambda "a" $ L.appl L.plus (L.appl (L.bvar 0) L.true))
      @?= A.val ( A.lambda [Name "a"] $ A.val $ A.lambda [DummyEnd] $ A.mkLet [(Nothing , A.appl (A.bvar 2) [A.true])] $ A.appl A.plus [A.bvar 0, A.bvar 1] )

   -- TODO wright forallAnormalform = aToLambda ANormalize
   ,  testProperty "wel formd" $ forAllUnTypedBruijn $ \t ->
      welFormd $ aToLambda $ aNormalize t

   , testProperty "normalised" $ forAllUnTypedBruijn $ \t ->
      normalised $ aToLambda $ aNormalize t

   , testProperty "keep type" $forAllTypedBruijn $ \e1 ->
      isRight $ do
         t1 <- solver e1
         let e2 = aToLambda $ aNormalize e1
         t2 <- solver e2
         return $ counterexample (
              "\nbefore: \n" ++ show e1 ++
            "\n\npShow:\n " ++ Lam.pShow e1 ++
              "\n\t::" ++ Type.pShow t1 ++
             "\n\nafter :\n" ++ show e2 ++
            "\n\npShow:\n " ++ Lam.pShow e2 ++
              "\n\t::" ++ Type.pShow t2)
             $ unifys t1 (Type.mapFree (\ (Free i) -> Free (i + 10000)) t2)
   ]
