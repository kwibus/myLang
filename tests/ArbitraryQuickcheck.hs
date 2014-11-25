{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryQuickcheck 
     where

import Lambda

import Test.QuickCheck.Gen
import Test.QuickCheck
import Data.Maybe
import Control.Applicative

instance Arbitrary (BruijnTerm a) where
    arbitrary = fmap lam2Bruijn arbitrary 
    shrink = shrinkBruijn

instance Arbitrary  (LamTerm a) where
    arbitrary = sized $ arbitraryTerm []
    shrink t = fmap bruijn2Lam $ shrink $ lam2Bruijn t

shrinkBruijn :: BruijnTerm a-> [BruijnTerm a]
shrinkBruijn (BAppl t1 t2) = [t1,t2]  ++
                             [BAppl t1' t2' | (t1',t2')<- shrink (t1,t2)]
shrinkBruijn (BLambda  n t)= fastShrink t ++
                             eliminated ++
                             fmap (BLambda  n) (shrinkBruijn t)
    where fastShrink (BVar (Bound {}))= []
          fastShrink _  = [BLambda n (BVar (Bound 0))]
          eliminated = maybeToList  $ eliminatedLambda 0 t
shrinkBruijn _ = []

eliminatedLambda :: Index -> BruijnTerm a -> Maybe (BruijnTerm a)
eliminatedLambda i1 (BVar (Bound i2))
    | i1==i2  = Nothing
    | i2 > i1 = Just $ BVar $ Bound (i2-1)
    | otherwise = Just $ BVar $  Bound i2
eliminatedLambda _ (t@BVar{}) = Just t
eliminatedLambda i (BLambda  n t) = BLambda n <$> eliminatedLambda (i-1) t
eliminatedLambda i (BAppl t1 t2) = BAppl<$> eliminatedLambda  i t1 <*>  eliminatedLambda i t2

arbitraryTerm :: [Name] -> Int -> Gen (LamTerm a)
arbitraryTerm [] 0 = arbitraryLambda [] 0
arbitraryTerm [] s = oneof [arbitraryLambda [] s,arbitraryAppl [] s ]
arbitraryTerm n s
    | s > 0 = frequency [(3,arbitraryLambda n s),
                         (1,arbitraryVar n),
                         (3,arbitraryAppl n s)
                        ]
    | otherwise = arbitraryVar n

arbitraryLambda :: [Name] -> Int -> Gen (LamTerm a)
arbitraryLambda names s =do
  boolNewName <- case names of
        [] -> return True
        _ -> frequency [(4,return True),(1,return False)]
  name <- if boolNewName
            then fmap(:[]) $ choose ('a','z')
            else elements names
  let newnames =  if boolNewName
        then name : names
        else  names
  term <- arbitraryTerm  newnames (s-1)
  return $ Lambda name term

arbitraryVar :: [Name ] -> Gen (LamTerm a)
arbitraryVar names =  do
  name <- elements names
  return $ Var $ VarVar name

arbitraryAppl :: [Name] -> Int -> Gen (LamTerm a)
arbitraryAppl names s = do
     t1 <- arbitraryTerm names $ s `div` 2
     t2 <- arbitraryTerm names $ s `div` 2
     return $ Appl t1 t2
