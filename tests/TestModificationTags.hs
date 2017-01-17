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
    [ testApplyModify
    , testReorderTag
    , testsubstituteTag
    , testCombined
    ]

-- TODO fixname
reorder' :: [Int] -> T.LamTerm () Bound (Modify ()) -> T.LamTerm () Bound (Modify ())
reorder' = T.Tag . Reorder . map Bound

testApplyModify :: TestTree
testApplyModify = testGroup "applyModify"
    [ testCase "\\a.b" $ applyModify (T.lambda "a" $ T.bvar 1) @?= lambda "a" ( bvar 1)
    , testCase "\\a.a" $ applyModify (T.lambda "a" $ T.bvar 0) @?= lambda "a" ( bvar 0)
    ]

testReorderTag :: TestTree
testReorderTag = testGroup "Reorder"
    [ testCase "remember" $
        remember (Reorder $ map Bound [1, 0, 2]) (insertT (map Undefined [2, 1, 0] :: [Symbol ()]) empty )
        @?= insertT (map Undefined [2, 0, 1]) empty

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

sub' :: BruijnTerm () -> T.LamTerm () Bound (Modify ()) -> T.LamTerm () Bound (Modify ())
sub' = T.Tag . Substitut

testsubstituteTag :: TestTree
testsubstituteTag = testGroup "sub'stitut"
    [ testCase "[a/b] a= b" $
        applyModify ((sub' $ bvar 0) (T.bvar 0)) @?= bvar 0
    , testCase "[a/1.0,b/2.0,b/3.0] b = 2" $
        applyModify (sub' (double 3) $ sub' (double 2) $ sub' (double 1) $ T.bvar 1)
        @?= double 2

    , testCase "[a/1.0] \\b.a = \\b.1.0" $
        applyModify (sub' (double 1) (T.lambda "b" $ T.bvar 1 )) @?= lambda "b" (double 1.0)

    , testCase "([a/1.0]a)a =1.0a" $
        applyModify (T.appl (sub' (double 1) $ T.bvar 0) (T.bvar 0)) @?= appl (double 1) (bvar 0)

    -- , testCase "[a/\\c.b,b/1,0] a = \\c.1.0" $
    --     let env = bFromList [lambda "c" (bvar 1),double 1]
    --     in sub'stituteEnv env  (bvar 1)  @?= lambda "c" (double 1.0)

    -- , testCase "[a/b.b/c] a = a" $
    --     let env = bFromList [bvar 0,bvar 2]
    --     in sub'stituteEnv env  (bvar 1)  @?= bvar 2

    , testCase "[a/c] \\b.a = \\b.c" $
        applyModify ( sub' (bvar 0) (T.lambda "b" $ T.bvar 1)) @?= lambda "b" (bvar 1)

    , testCase "[a/c] \\b.c = \\b.c" $
        applyModify ( sub' (bvar 0) (T.lambda "b" $ T.bvar 2)) @?= lambda "b" (bvar 1)

    , testCase "[a/c] \\b.d = \\b.d" $
        applyModify ( sub' (bvar 0) (T.lambda "b" $ T.bvar 3)) @?= lambda "b" (bvar 2)

    , testCase "[a/d]\\b.[c/d]ac = \\b.dd" $
        applyModify (sub' (bvar 0) $ T.lambda "b" $ sub' (bvar 2 ) $ T.appl (T.bvar 2) (T.bvar 0))
        @?= lambda "b" ( appl (bvar 1) (bvar 1))

    , testCase "[a/d]\\b.[c/e]ac = \\b.de" $
        applyModify (sub' (bvar 0) $ T.lambda "b" $ sub' (bvar 3 ) $ T.appl (T.bvar 2) (T.bvar 0))
        @?= lambda "b" ( appl (bvar 1) (bvar 2))

    ]

testCombined :: TestTree
testCombined = testGroup "sub'stitut and Reorder combined"
    [ testCase "[a/d]\\b. [b<->a] [c/e]ac = \\b.ba" $
        applyModify (sub' (bvar 0) $
                T.lambda "b" $ reorder' [1, 0] $
                sub' (bvar 2) $
                T.appl (T.bvar 2) (T.bvar 0))
        @?= lambda "b" ( appl (bvar 0) (bvar 1))


    , testCase "[a/d]\\b. [b<->a] [c/b]ac = \\b.ba" $
        applyModify (sub' (bvar 0) $
                T.lambda "b" $ reorder' [1, 0] $
                sub' (bvar 0) $
                T.appl (T.bvar 2) (T.bvar 0))
        @?= lambda "b" ( appl (bvar 0) (bvar 1))
    ]
