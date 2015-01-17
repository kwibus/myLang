{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances #-}
module ArbitraryQuickcheck
     where

import Test.QuickCheck.Gen
import Test.QuickCheck
import Data.Maybe
import Control.Applicative

import Vallue
import BruijnTerm
import Lambda
import ArbitraryVallue

instance  Arbitrary (BruijnTerm Vallue) where
    arbitrary = fmap lam2Bruijn arbitrary
    shrink = shrinkBruijn

instance Arbitrary (LamTerm Vallue Name) where
    arbitrary = sized $ arbitraryTerm []
    shrink t = fmap bruijn2Lam $ shrink $ lam2Bruijn t

shrinkBruijn :: BruijnTerm Vallue -> [BruijnTerm Vallue ]
shrinkBruijn (Appl t1 t2) = [t1, t2] ++
                             [Appl t1' t2' | (t1', t2') <- shrink (t1, t2)]
shrinkBruijn (Lambda n t) = fastShrink t ++
                             eliminated ++
                             fmap (Lambda n) (shrinkBruijn t)
    where fastShrink (Var {}) = []
          fastShrink _ = [Val (MyDouble 1.0)]
          eliminated = maybeToList $ eliminatedLambda 0 t
shrinkBruijn _ = []

eliminatedLambda :: Index -> BruijnTerm Vallue -> Maybe (BruijnTerm Vallue)
eliminatedLambda i1 (Var i2)
    | i1 == i2 = Nothing
    | i2 > i1 = Just $ Var (i2 - 1)
    | otherwise = Just $ Var i2
eliminatedLambda _ (t@Val {}) = Just t
eliminatedLambda i (Lambda n t) = Lambda n <$> eliminatedLambda (i - 1) t
eliminatedLambda i (Appl t1 t2) = Appl <$> eliminatedLambda i t1 <*> eliminatedLambda i t2

arbitraryTerm :: [Name] -> Int -> Gen (LamTerm Vallue Name)
arbitraryTerm [] 0 = oneof [arbitraryLambda [] 0, arbitraryVallue ]
arbitraryTerm [] s = oneof [arbitraryLambda [] s, arbitraryAppl [] s ]
arbitraryTerm n s
    | s > 0 = frequency [(9, arbitraryLambda n s)
                        , (2, arbitraryVar n)
                        , (1, arbitraryVallue)
                        , (9, arbitraryAppl n s)
                        ]
    | otherwise = arbitraryVar n

arbitraryLambda :: [Name] -> Int -> Gen (LamTerm Vallue Name)
arbitraryLambda names s = do
  boolNewName <- case names of
        [] -> return True
        _ -> frequency [(4, return True), (1, return False)]
  name <- if boolNewName
            then fmap (: []) $ choose ('a', 'z')
            else elements names
  let newnames = if boolNewName
        then name : names
        else names
  term <- arbitraryTerm newnames (s - 1)
  return $ Lambda name term

arbitraryVar :: [Name ] -> Gen (LamTerm Vallue Name)
arbitraryVar names = do
  name <- elements names
  return $ Var name

arbitraryAppl :: [Name] -> Int -> Gen (LamTerm Vallue Name )
arbitraryAppl names s = do
     t1 <- arbitraryTerm names $ s `div` 2
     t2 <- arbitraryTerm names $ s `div` 2
     return $ Appl t1 t2
