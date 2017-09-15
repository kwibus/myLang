module TestModify where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import ArbitraryLambda
import MTable
import ModificationTags (applyModify,remember,Modify(..))
import Inprocess (proces)
import qualified Inprocess as P

import qualified TaggedLambda as T
import BruijnEnvironment
import BruijnTerm
import qualified MakeTagedTerm as T
import MakeTerm
import TestUtils

-- TODO consider testing (table,ast) instead of taged ast

testModify :: TestTree
testModify = testGroup "modifcation tags"
    [ testApplyModify
    , testReorderTag
    , testsubstituteTag
    , testIncFreeTag
    , testCombined
    ]

testIntermidiats :: String -> T.LamTerm () Bound (Modify ()) -> LamTerm () Bound -> TestTree
testIntermidiats discription input result =
    amplify discription
            (\(inprocess,mtable ) -> proces mtable inprocess @?= result)
            amplified
    where
      amplified = [(P.Unproc input,empty)
                  -- , first Inproc  $ peek empty (Unproc input) -- FIXME
                  , (P.New $ T.tag $ applyModify input,empty)]

-- TODO fixname
reorder' :: [Int] -> T.LamTerm () Bound (Modify ()) -> T.LamTerm () Bound (Modify ())
reorder' = T.Tag . Reorder 0 . map Bound

testApplyModify :: TestTree
testApplyModify = testGroup "applyModify"
    [ testIntermidiats "\\a.b" (T.lambda "a" $ T.bvar 1) (lambda "a" ( bvar 1))
    , testIntermidiats "\\a.a" (T.lambda "a" $ T.bvar 0) (lambda "a" ( bvar 0))
    ]

testReorderTag :: TestTree
testReorderTag = testGroup "Reorder"
    [ testCase "remember" $
        remember (Reorder 0 $ map Bound [1, 0, 2]) (insertUndefined  3 empty )
        @?= MTable {getDepth = 3, incFreeFromStart = 0, getEnv = bFromList (map Undefined [0, 2, 1])}

    , testCase "\\a,[] a" $
        applyModify (T.lambda "a" $ reorder' [0] $ T.bvar 0)
        @?= lambda "a" ( bvar 0)

    , testCase "let a = 1; b = 2; c=3 in [a-> b, b->a, c->c] abc" $
        applyModify
        (T.mkLet [("a", T.double 1), ("b", T.double 2), ("c", T.double 3)] $
        reorder' [1, 0, 2] $ T.appl (T.appl (T.bvar 0) (T.bvar 1 )) (T.bvar 2))
         @?= mkLet [("a", double 1), ("b", double 2), ("c", double 3)]
                 (appl (appl (bvar 1) (bvar 0)) (bvar 2))

    , testCase "\\a b. [a<->b]ab" $
        applyModify
        (T.lambda "a" $ T.lambda "b" $
        reorder' [1, 0] $ T.appl (T.bvar 0) (T.bvar 1 ))
        @?=
        lambda "a" ( lambda "b" $ appl (bvar 1) (bvar 0))

    , testCase "\\a b.[a<->b] \\c.abc" $
        applyModify (
            T.lambda "a" $ T.lambda "b" $ reorder' [1, 0] $ T.lambda "c" $
            T.appl (T.appl (T.bvar 0) (T.bvar 1 )) (T.bvar 2))
        @?=
        lambda "a" ( lambda "b" $ lambda "c" $
        appl (appl (bvar 0) (bvar 2 )) (bvar 1))

    -- , testCase "reorder' non existent" $
    --     applyModify (T.lambda "a" $ reorder' [1,0] $ T.bvar 0)
    --     @?= lambda "a" ( bvar 1)
    ]

incFree' :: Int -> T.LamTerm () Bound (Modify ()) -> T.LamTerm () Bound (Modify ())
incFree' = T.Tag . IncFree 0

testIncFreeTag :: TestTree
testIncFreeTag = testGroup "incfree"
    [ -- TODO  other ast are not tested (Unprocessed inprogross ..)
        testProperty "applyModify tag-incfree  ast == BruijnTerm.incfree n ast " $
        forAllUnTypedBruijn $ \ t ->
        applyModify (incFree' 5 $ T.tag t) === BruijnTerm.incFree 5 t
    , testIntermidiats "incfree \\a.ab"
        (incFree' 5 $ T.lambda "a" $ T.appl (T.bvar 0) (T.bvar 1))
        (lambda "a" $ appl (bvar 0) (bvar 6))
    ]

sub' :: T.LamTerm () Bound (Modify ()) -> T.LamTerm () Bound (Modify ()) -> T.LamTerm () Bound (Modify ())
sub' = T.Tag . SubstitutT 0

testsubstituteTag :: TestTree
testsubstituteTag = testGroup "sub'stitut"
    [ testIntermidiats "[a/b] a= b"
        ((sub' $ T.bvar 0) (T.bvar 0))
        (bvar 0)
    , testIntermidiats "[a/1.0,b/2.0,b/3.0] b = 2"
        (sub' (T.double 3) $ sub' (T.double 2) $ sub' (T.double 1) $ T.bvar 1)
        (double 2)

    , testIntermidiats "[a/1.0] \\b.a = \\b.1.0"
        (sub' (T.double 1) (T.lambda "b" $ T.bvar 1 ))
        (lambda "b" (double 1.0))

    , testIntermidiats "([a/1.0]a)a =1.0a"
        (T.appl (sub' (T.double 1) $ T.bvar 0) (T.bvar 0))
        (appl (double 1) (bvar 0))

    , testIntermidiats "[b/1] [a/b] \\c.a = \\b.1.0"
        (sub' (T.double 1) $ sub' (T.bvar 0) (T.lambda "b" $ T.bvar 1 ))
        (lambda "b" (double 1.0))

    -- , testCase "[a/\\c.b,b/1,0] a = \\c.1.0" $ --FIXME
    --     let env = bFromList [lambda "c" (bvar 1),double 1]
    --     in sub'stituteEnv env  (bvar 1)  @?= lambda "c" (double 1.0)

    -- , testCase "[a/b.b/c] a = a" $
    --     let env = bFromList [bvar 0,bvar 2]
    --     in sub'stituteEnv env  (bvar 1)  @?= bvar 2

    , testIntermidiats "[a/c] \\b.a = \\b.c"
        ( sub' (T.bvar 0) (T.lambda "b" $ T.bvar 1))
        (lambda "b" (bvar 1))

    , testIntermidiats "[a/c] \\b.c = \\b.c"
        (sub' (T.bvar 0) (T.lambda "b" $ T.bvar 2))
        (lambda "b" (bvar 1))

    , testIntermidiats "[a/c] \\b.d = \\b.d"
        (sub' (T.bvar 0) (T.lambda "b" $ T.bvar 3))
        (lambda "b" (bvar 2))

    , testIntermidiats "[a/d]\\b.[c/d]ac = \\b.dd"
        (sub' (T.bvar 0) $ T.lambda "b" $ sub' (T.bvar 2 ) $ T.appl (T.bvar 2) (T.bvar 0))
        (lambda "b" ( appl (bvar 1) (bvar 1)))

    , testIntermidiats "[a/d]\\b.[c/e]ac = \\b.de"
        (sub' (T.bvar 0) $ T.lambda "b" $ sub' (T.bvar 3 ) $ T.appl (T.bvar 2) (T.bvar 0))
        (lambda "b" ( appl (bvar 1) (bvar 2)))

    ]

testCombined :: TestTree
testCombined = testGroup "sub'stitut and Reorder combined"
    [ testIntermidiats "[a/true,b/a] [a<->b] b = true"
        (sub' T.true $ sub' (T.bvar 0) $ reorder' [1, 0] $ T.bvar 1 )
        true

    , testIntermidiats "[a/d]\\b. [b<->a] [c/e]ac = \\b.ba"
        (sub' (T.bvar 0) $
                T.lambda "b" $ reorder' [1, 0] $
                sub' (T.bvar 2) $
                T.appl (T.bvar 2) (T.bvar 0))
        (lambda "b" ( appl (bvar 0) (bvar 1)))

    , testIntermidiats "[a/d]\\b. [b<->a] [c/b]ac = \\b.ba"
        (sub' (T.bvar 0) $
                T.lambda "b" $ reorder' [1, 0] $
                sub' (T.bvar 0) $
                T.appl (T.bvar 2) (T.bvar 0))
        (lambda "b" ( appl (bvar 0) (bvar 1)))
    ]
