module TestModify
    (testModify)
where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import ArbitraryLambda
import MTable

import ModificationTags (applyModify,Modify(..))
import qualified ModificationTags as M (LamTerm())
import qualified ModificationTags
import qualified Modify

import qualified TaggedLambda as T (tag,LamTerm (Tag))
import BruijnEnvironment
import BruijnTerm
import qualified MakeTagedTerm as T
import MakeTerm

-- TODO test inprocess and maybe Unprocessed

testModify :: TestTree
testModify = testGroup "modifcation tags"
    [ testMTable
    , testModifyBruijnTerm
    , testModificationsTags
    ]

testMTable :: TestTree
testMTable = testGroup "mtable"
    [ testCase "[1,0,2]" $
        (reorder 0 (map Bound [1, 0, 2] ) $ insertUndefined 3 empty )
        @?= (mTable 3 0){ _env = bFromList (map (\d ->(d,Undefined)) [0, 2, 1])}
    ]

testModifyBruijnTerm :: TestTree
testModifyBruijnTerm = testGroup "Modify BruijnTerm"
    [ testSet "id" testSetId
    , testSet "incFree" testSetIncFree
    , testSet "substitute" testSubstitueSet
    , testSet "reorder" testReorderSet

    , testGroup "propertys"
        [ testProperty "applyModify tag-incfree  ast == BruijnTerm.incfree n ast " $
            forAllUnTypedBruijn $ \ t ->
            Modify.proces (MTable.incFree 5 empty)  t === BruijnTerm.incFree 5 t
        ]
    ]
  where
    testSet str set = testGroup str $ map testTupl set
    testTupl (str, m, input, result) = testCase str $ Modify.proces m input @?= result

testModificationsTags :: TestTree
testModificationsTags = testGroup "ModificationTags"
    [ testSet "id" testSetId
    , testSet "incFree" testSetIncFree
    , testSet "substitute" testSubstitueSet
    , testSet "reorder" testReorderSet
    , testTags "tags" testTagsSet
    , testTags "combined tags" testTagsCobinedSet
    , testGroup "propertys"
        [ testProperty "applyModify tag-incfree  ast == BruijnTerm.incfree n ast " $
            forAllUnTypedBruijn $ \ t ->
            applyModify (incFree' 5 $ T.tag t) === BruijnTerm.incFree 5 t
        ]
    ]
  where
    testTags str set = testGroup str $
        map (\(discription, input,result)-> testCase discription $ applyModify  input@?= result) set
    testSet str set = testGroup str $ map testTupl set
    testTupl (str, m, input, result) = testCase str $ ModificationTags.proces m (T.tag input) @?= result


testSetId :: [(String, MTable, BruijnTerm (), BruijnTerm ())]
testSetId = map (\t -> (pShow t, empty,t,t))
    [ lambda "a" $ bvar 1
    , lambda "a" $ bvar 0]

mkAbc :: Int -> BruijnTerm ()
mkAbc n = foldl1 appl $ map bvar [0..n]

testSetIncFree :: [(String, MTable, BruijnTerm () , BruijnTerm ())]
testSetIncFree =
    [ ("\\a.ab", mTable 0 5
        , lambda "a" $ appl (bvar 0) (bvar 1)
        , lambda "a" $ appl (bvar 0) (bvar 6))
    , ("\\b\\a.abc", mTable 0 5
        , lambda "b" $ lambda "a" $ appl (appl (bvar 0) (bvar 1)) (bvar 2)
        , lambda "b" $ lambda "a" $ appl (appl (bvar 0) (bvar 1)) (bvar 7))

    , let abc  = mkAbc 2
          abc' = mkAbc 1 `appl` bvar 7
      in ("let b = abc; a = abc in abc", mTable 0 5
        , mkLet [("a",abc),("b",abc)] abc
        , mkLet [("a",abc'),("b",abc')] abc')
    ]

reorderTable :: [Int] -> MTable -> MTable
reorderTable order m = reorder 0 (map Bound order )$ insertUndefined (length order) m

testReorderSet :: [(String,MTable,BruijnTerm () , BruijnTerm ())]
testReorderSet =
    [ ("[] a" , reorderTable [] empty , bvar 0 , bvar 0)
    , ("[a->a] a" , reorderTable [0] empty , bvar 0 , bvar 0)
    , ("[a-> b, b->a, c->c] a" , reorderTable [1,0,2] empty , bvar 0 , bvar 1)
    , ("let a = 1; b = 2; c=3 in [a-> b, b->a, c->c] abc"
            , reorderTable [1,0,2] empty
            , mkAbc 2
            , appl (appl (bvar 1) (bvar 0)) (bvar 2))

    -- -- , testCase "reorder' non existent" $
    -- --     applyModify (T.lambda "a" $ reorder' [1,0] $ T.bvar 0)
    -- --     @?= lambda "a" ( bvar 1)
    ]


substitutTable :: [BruijnTerm ()] -> MTable -> MTable
substitutTable  terms m = foldl (flip (substitute (Bound 0) 0)) m terms

testSubstitueSet :: [(String,MTable,BruijnTerm () , BruijnTerm ())]
testSubstitueSet =
    [ ("[a/b] a= b", substitutTable [bvar 0] empty, bvar 0, bvar 0)
    , ("[a/c] a= b", substitutTable [bvar 1] empty, bvar 1, bvar 0)
    , ("[a/1.0,b/2.0,b/3.0] b = 2", substitutTable (map double [1..3]) empty, bvar 1 , double 2)
    , ("[a/1.0] \\b.ab = \\b.1.0b", substitutTable [double 1] empty
        , lambda "b" $ appl (bvar 1)(bvar 0)
        , lambda "b" $ appl (double 1.0) (bvar 0))

    , ("[b/c] \\a.abc = \\b.c", substitutTable [bvar 0] empty
        , lambda "b" $ mkAbc 2
        , lambda "b" $ bvar 0 `appl` bvar 1 `appl` bvar 1)

    -- , testCase "[a/\\c.b,b/1,0] a = \\c.1.0" $ --FIXME
    --     let env = bFromList [lambda "c" (bvar 1),double 1]
    --     in sub'stituteEnv env  (bvar 1)  @?= lambda "c" (double 1.0)

    -- , testCase "[a/b.b/c] a = a" $
    --     let env = bFromList [bvar 0,bvar 2]
    --     in sub'stituteEnv env  (bvar 1)  @?= bvar 2
    ]

-- TODO fixname
reorder' :: [Int] -> M.LamTerm () -> M.LamTerm ()
reorder' = T.Tag . Reorder 0 . map Bound

incFree' :: Int -> M.LamTerm () -> M.LamTerm ()
incFree' = T.Tag . IncFree 0

sub' :: M.LamTerm () -> M.LamTerm () -> M.LamTerm ()
sub' = T.Tag . SubstitutT 0

testTagsSet :: [(String, M.LamTerm  (), BruijnTerm())]
testTagsSet =
    [ ("\\a b.[a<->b] \\c.abc"
        , T.lambda "a" $ T.lambda "b" $ reorder' [1, 0] $ T.lambda "c" $
          T.appl (T.appl (T.bvar 0) (T.bvar 1 )) (T.bvar 2)

        ,lambda "a" ( lambda "b" $ lambda "c" $
            appl (appl (bvar 0) (bvar 2 )) (bvar 1)))

    , ("([a/1.0]a)a =1.0a"
        , T.appl (sub' (T.double 1) $ T.bvar 0) (T.bvar 0)
        ,appl (double 1) (bvar 0))

    , ("[b/1] [a/b] \\c.a = \\b.1.0"
        , sub' (T.double 1) $ sub' (T.bvar 0) (T.lambda "b" $ T.bvar 1 )
        , lambda "b" (double 1.0))

    , ("[a/d]\\b.[c/d]acd = \\b.ddd"
        , sub' (T.bvar 0) $ T.lambda "b" $ sub' (T.bvar 2 ) $ T.appl (T.appl (T.bvar 2) (T.bvar 0)) (T.bvar 3)
        , lambda "b" $ appl (appl (bvar 1) (bvar 1)) (bvar 1))

    , ("[a/d]\\b.[c/e]acd = \\b.ded"
        , sub' (T.bvar 0) $ T.lambda "b" $ sub' (T.bvar 3 ) $ T.appl (T.appl (T.bvar 2) (T.bvar 0)) (T.bvar 3)
        , lambda "b" $ appl (appl (bvar 1) (bvar 2)) (bvar 1))

    , ("[a/d]\\b.[c/a]acd = \\b.ddd"
        , sub' (T.bvar 0) $ T.lambda "b" $ sub' (T.bvar 1 ) $ T.appl (T.appl (T.bvar 2) (T.bvar 0)) (T.bvar 3)
        , lambda "b" $ appl (appl (bvar 1) (bvar 1)) (bvar 1))
    ]


testTagsCobinedSet :: [(String, M.LamTerm  (), BruijnTerm())]
testTagsCobinedSet =
    [ ("[a/true,b/a] [a<->b] b = true"
        , sub' T.true $ sub' (T.bvar 0) $ reorder' [1, 0] $ T.bvar 1
        , true)

    , ("[a/d]\\b. [b<->a] [c/e]ac = \\b.ba"
        , sub' (T.bvar 0) $
                T.lambda "b" $ reorder' [1, 0] $
                sub' (T.bvar 2) $
                T.appl (T.bvar 2) (T.bvar 0)
        , lambda "b" ( appl (bvar 0) (bvar 1)))

    , ("[a/d]\\b. [b<->a] [c/b]ac = \\b.ba"
        , sub' (T.bvar 0) $
                T.lambda "b" $ reorder' [1, 0] $
                sub' (T.bvar 0) $
                T.appl (T.bvar 2) (T.bvar 0)
        , lambda "b" ( appl (bvar 0) (bvar 1)))
    ]
