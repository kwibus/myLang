module ArbitraryQuickcheck where

import Lambda
import Test.QuickCheck.Gen
import Test.QuickCheck
-- import Debug.Trace

instance Arbitrary BruijnTerm where
    arbitrary = fmap lam2Bruijn arbitrary
    shrink = shrinkBruijn
instance Arbitrary  LamTerm where
    arbitrary = resize 100 $sized $ arbitraryTerm []

shrinkBruijn :: BruijnTerm -> [BruijnTerm]
shrinkBruijn (BAppl t1 t2) = [t1,t2]  ++ [BAppl t1' t2' | (t1',t2')<- shrink (t1,t2)]
shrinkBruijn (BLambda  n t)= fastShrink t ++eliminated ++ fmap (BLambda  n) (shrinkBruijn t)
    where fastShrink (Bound {})= []
          fastShrink _  = [BLambda n (Bound 0)]
          eliminated = if used 0 t then [] else [t]
shrinkBruijn __ = []

used :: Index -> BruijnTerm -> Bool
used i1 (Bound i2) = i1==i2
used i (BLambda  _ t) =  used (i+1) t
used i (BAppl t1 t2) = used i t1 || used i  t2

arbitraryTerm :: [Name] -> Int -> Gen LamTerm
arbitraryTerm [] s = oneof [arbitraryLambda [] s, arbitraryAppl [] s]
arbitraryTerm n s 
    | s /= 0 = frequency [(3,arbitraryLambda n s), (1,arbitraryVar n) ,(1,arbitraryAppl n s)]
    | otherwise = arbitraryVar n

arbitraryLambda :: [Name] -> Int -> Gen LamTerm
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
  term <- arbitraryTerm  newnames (s+1)
  return $ Lambda name term

arbitraryVar :: [Name ] -> Gen LamTerm
arbitraryVar names =  do
  name <- elements names
  return $ Var name

arbitraryAppl :: [Name] -> Int -> Gen LamTerm 
arbitraryAppl names s = do
     t1 <- arbitraryTerm names $ s `div` 2
     t2 <- arbitraryTerm names $ s `div` 2
     return $ Appl t1 t2
