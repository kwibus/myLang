module TestModificationTags where

import Test.Tasty
import Test.Tasty.HUnit
import ModificationTags
import qualified TaggedLambda as T
import BruijnEnvironment
import BruijnTerm
import qualified MakeTagedTerm as T
import MakeTerm

testModificationTags :: TestTree
testModificationTags = testGroup "modifcation tags"
    [testReorderTag ,testSubstituteTag,testCombined]

reorder :: [Int] -> T.LamTerm () Bound (Modify ()) -> T.LamTerm () Bound (Modify ())
reorder = T.Tag . Reorder . map Bound

testReorderTag :: TestTree
testReorderTag = testGroup "Reorder"
    [ testCase "rember" $
        rember (Reorder $ map Bound [1,0,2]) (bFromList [Keep 2,Keep 1,Keep 0:: Ref ()])
        @?= bFromList [Keep 2,Keep 0,Keep 1]

    , testCase "reorder1" $
        proces (T.lambda "a" $ reorder [0] $ T.bvar 0)
        @?= lambda "a" ( bvar 0)

    , testCase "reorder2" $
        proces
        (T.mkLet [("a",T.double 1),("b",T.double 2),("c",T.double 3)] $
        reorder [1,0,2] $ T.appl (T.appl (T.bvar 0) (T.bvar 1 )) (T.bvar 2))
         @?=
        mkLet [("a",double 1),("b",double 2),("c",double 3)] ( appl (appl (bvar 1) (bvar 0)) (bvar 2))

    , testCase "\\a b. [a<->b]ab" $
        proces
        (T.lambda "a" $ T.lambda "b" $
        reorder [1,0] $ T.appl (T.bvar 0) (T.bvar 1 ))
        @?=
        lambda "a" ( lambda "b" $ appl (bvar 1) (bvar 0))

    -- , testCase "reorder non existent" $
    --     proces (T.lambda "a" $ reorder [1,0] $ T.bvar 0)
    --     @?= lambda "a" ( bvar 1)
    ]

sub :: BruijnTerm () -> T.LamTerm () Bound (Modify ()) -> T.LamTerm () Bound (Modify ())
sub = T.Tag .Substitut

testSubstituteTag :: TestTree
testSubstituteTag = testGroup "Substitut"
    [ testCase "[a/1.0,b/2.0,b/3.0] b = 2" $
        proces (sub (double 3)$ sub (double 2) $ sub (double 1) $ T.bvar 1)  @?= double 2

    , testCase "[a/1.0] \\b.a = \\b.1.0" $
        proces (sub (double 1) (T.lambda "b" $ T.bvar 1 )) @?= lambda "b" (double 1.0)

    , testCase "([a/1.0]a)a =1.0a" $
        proces (T.appl (sub (double 1) $ T.bvar 0) (T.bvar 0)) @?= appl (double 1)(bvar 0)

    -- -- , testCase "[a/\\c.b,b/1,0] a = \\c.1.0" $
    -- --     let env = bFromList [lambda "c" (bvar 1),double 1]
    -- --     in substituteEnv env  (bvar 1)  @?= lambda "c" (double 1.0)
    --
    -- -- , testCase "[a/b.b/c] a = a" $
    -- --     let env = bFromList [bvar 0,bvar 2]
    -- --     in substituteEnv env  (bvar 1)  @?= bvar 2
    --
    , testCase "[a/c] \\b.a = \\b.c" $
        proces ( sub (bvar 1) (T.lambda "b" $ T.bvar 1)) @?= lambda "b" (bvar 2)

    , testCase "[a/c] \\b.a = \\b.d" $
        proces ( sub (bvar 1) (T.lambda "b" $ T.bvar 2)) @?= lambda "b" (bvar 1)

    , testCase "[a/d]\\b.[c/e]ac = \\b.de" $
        proces (sub (bvar 0) $ T.lambda "b" $ sub (bvar 2 ) $ T.appl (T.bvar 2) (T.bvar 0))
        @?= lambda "b" ( appl (bvar 1) (bvar 2))
    ]

testCombined :: TestTree
testCombined = testGroup "Substitut and Reorder combined"
    [ testCase "[a/d]\\b. [b<->a] [c/e]ac = \\b.be" $
        proces (sub (bvar 0) $
                T.lambda "b" $ reorder  [1,0] $
                sub (bvar 2) $
                T.appl (T.bvar 2) (T.bvar 0))
        @?= lambda "b" ( appl (bvar 0) (bvar 2))
    ]
