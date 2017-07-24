{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestSearchTree where

import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers hiding (checkBatch)

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Logic
import SearchTree
import Control.Monad.Identity

pruneI :: Int -> SearchTree Identity a -> [a]
pruneI maxfail = runIdentity . pruneT maxfail

foundI :: SearchTree Identity a -> [a]
foundI = runIdentity . foundT

testSearchTree:: TestTree
testSearchTree= testGroup "SearchTree" [testBacktrack,testSplit,testArbiSearchTree, checkRules]

-- if there are max  n failures in a row return True
failures :: Int -> SearchTree Identity a -> Bool
failures n tree = null (pruneI n succes) && not ( null  (pruneI (n+1)  succes))
    where succes = mplus tree (return undefined)

testBacktrack :: TestTree
testBacktrack = testGroup "backtrack"
  [ testBackJumps
  , testBackSteps2
  , testLimitBacksteps
  , testSetSearchTree
  , nestedMzero
  , stopMzero
  ]
-- TODO reworth backseps backjumps
testBackJumps :: TestTree
testBackJumps = testGroup "backjumps"
  [ testCase "backjump" $ pruneI 3
    (Search $ TreeT $ Identity $ Node
        [ Node
            [ Node
                [ Leaf Nothing
                , Leaf Nothing
                , Leaf Nothing
                , return (Just 1)
                ]
            , return (Just 2)
            ]
        , return (Just 3)
        ] )  @?=  [2,3:: Int]

  , testCase "backjumps 2 time" $ pruneI 3
    (Search $ TreeT $ Identity $ Node
        [ Node
            [ Node
                [ Leaf Nothing
                , Leaf Nothing
                , Leaf Nothing
                , return (Just 1)
                ]

            , Leaf Nothing
            , Leaf Nothing
            , Leaf Nothing
            , return (Just 2)
            ]
        , return (Just 3)
        ] )  @?=  [3:: Int]

  , testCase "early fail" $ pruneI 3
    (Search $ TreeT $ Identity $ Node

        [ Node
            [ Leaf Nothing
            , Leaf Nothing
            , Leaf Nothing]
        , Node
            [ Node
                [ Leaf Nothing
                , return (Just 1)
                ]
            , Leaf Nothing
            , return (Just 2)
            ]
        , return (Just 3)
        ] )  @?=  [1,2,3:: Int]
  -- , testCase "failure" $ pruneI 3
  --   (Search $ TreeT $ Identity $
  --      let failure = Node [Leaf Nothing,failure]
  --      in failure
  --   )  @?= ([] :: [Int])

  ]

testBackSteps2 :: TestTree
testBackSteps2 = testProperty "prune is same as found" $
    \s -> foundI s === pruneI 1000000000 (s :: SearchTree Identity Int)

testLimitBacksteps :: TestTree
testLimitBacksteps = testCase "limitBackSteps" $
   let g _ = f  (5::Int)
       f y = if y >= 0
             then tryM (map f [0 .. y - 1]):: SearchTree Identity Int else mzero
   in pruneI 3 ( g =<< try [0 :: Int, 1,2,3,4,5,6,7,8,9,10,undefined ] ) @?= []

-- assertFail :: String -> a -> String -> TestTree
-- assertFail nameTest a exception = testCase nameTest $
--  do
--     result <- Control.Exception.try (Control.Exception.evaluate a)
--     case result of
--      Right _ -> assertFailure ("test should fail with: " ++ exception)
--      Left e -> unless (show (e :: Control.Exception.ErrorCall) == exception) $
--             assertFailure ("test faild with: " ++ show e ++
--                       "\nexpect fail with: " ++ exception)

testSetSearchTree :: TestTree
testSetSearchTree = testGroup "from Set" $ map tester setSearchTree
  where tester (monadConstructed,expected, list, failN) = testGroup (show expected) [test1,test2,test3]
         where
           test1 = testCase "expeceted in monad notation"  (monadConstructed @?= expected)
           test2 = testCase "corect found list" $ foundI expected  @?= list
           test3 = testCase "correct numbermax failueres" $ assertBool "" $ failures failN expected

setSearchTree :: [(SearchTree Identity Int ,SearchTree Identity Int,[Int],Int)]
setSearchTree =
  [ ( do x <- try [1 .. 3 :: Int]
         guard (x == 4)
         return x
  , Search $ TreeT $ Identity $ Node
        [ Leaf Nothing
        , Leaf Nothing
        , Leaf Nothing
        ]
    , [] , 3)
  , ( do x <- try [1 .. 3 :: Int]
         guard (x == 3)
         return x
    , Search $ TreeT $ Identity $ Node
        [ Leaf Nothing
        , Leaf Nothing , Leaf ( Just 3)]
    , [3] , 2)
  ]

stopMzero :: TestTree
stopMzero = testCase "stop ad mzero" $ assertBool "" $ failures 1 (do
  i <- try []
  tryM [try [i], mzero])

nestedMzero :: TestTree
nestedMzero = testCase "nested mzero " $ assertBool "" $ failures 2(
  tryM [try [], mzero])

checkBatch  :: TestBatch -> TestTree
checkBatch (name,tests)= testGroup name $ map checkTest tests
  where checkTest (string,prop) = testProperty string prop

instance (Eq (m [a]), Functor m) => EqProp (SearchTree m a ) where
    (=-=) a  b = eq (foundT a) (foundT b)

instance (Arbitrary a, Monad m) => Arbitrary (SearchTree m a) where
    arbitrary = fmap (Search . TreeT . return) arbitrary

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = sized sizedArbi
    where -- sizedArbi :: Int -> Gen (Tree ( Maybe a))
      sizedArbi n
        | n <= 1    = Leaf <$> arbitrary
        | otherwise = Node <$> do
            sizes <- split (n-1)
            mapM sizedArbi sizes

-- TODO consider more fair disrobution
split :: Int -> Gen [Int]
split 0 = return []
split 1 = return [1]
split n = do
    i <- choose (1, n`div`2)
    fmap (i :)  (split $! n -i)

testSplit :: TestTree
testSplit = testProperty "check size split" $
   forAll (suchThat (arbitrary :: Gen Int) (>= 0)) (\ n ->
        (forAll (split n) $ \list -> sum list === n))

testArbiSearchTree :: TestTree
testArbiSearchTree = testProperty "check size Arbitrary SearchTree" $
   forAll (suchThat (arbitrary :: Gen Int) (> 1)) (\ n ->
        (forAll (resize n arbitrary) (\ t -> size ( runIdentity$  run $ search (t::SearchTree Identity Int) ) === n)))
  where
    size :: Tree a -> Int
    size Leaf {} = 1
    size (Node trees) = 1+sum ( map size trees)

checkRules :: TestTree
checkRules = testGroup "test TypeClass Rules"
    [ -- the check for monad associativiy is slow, you get same if you run it with listT
      -- the bottleneck is in generation of arbitrary data
      -- this just makes size of arbitrary smaller so it will not blow up
      adjustOption (\(QuickCheckMaxSize i) -> QuickCheckMaxSize i`div`4) $
        checkBatch $ monad (undefined :: SearchTree Identity (Int,Bool,Double))
    ,
    checkBatch $ monadFunctor (undefined :: SearchTree Identity (Int,Bool))
    , checkBatch $ monadApplicative (undefined :: SearchTree Identity (Int,Bool))
    , checkBatch $ monadPlus (undefined :: SearchTree Identity (Int,Bool))
    , checkBatch $ alternative (undefined :: SearchTree Identity Int)
    ]
