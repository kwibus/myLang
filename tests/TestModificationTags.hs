module TestModificationTags where

import Test.Tasty
import Test.Tasty.HUnit
import ModificationTags
import TaggedLambda
import BruijnEnvironment

import qualified MakeTagedTerm as T
import MakeTerm

testModificationTags :: TestTree
testModificationTags = testGroup "modifcation tags"
    [ testCase "rember" $
        rember (Reorder $ map Bound [1,0,2]) (bFromList [2,1,0::Int])
        @?= bFromList [2,0,1::Int]

    , testCase "reorder1" $
        proces (T.lambda "a" $ Tag (Reorder [Bound 0]) $ T.bvar 0)
        @?= lambda "a" ( bvar 0)

    , testCase "reorder2" $
        proces
        (T.mkLet [("a",T.double 1),("b",T.double 2),("c",T.double 3)] $
        Tag (Reorder [Bound 1, Bound 0, Bound 2]) $ T.appl (T.appl (T.bvar 0) (T.bvar 1 )) (T.bvar 2))
         @?=
        mkLet [("a",double 1),("b",double 2),("c",double 3)] ( appl (appl (bvar 1) (bvar 0)) (bvar 2))
    ]
