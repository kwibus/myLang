module ArbitraryQuickcheck where

import Lambda
import Test.QuickCheck.Gen
import Test.QuickCheck
import Debug.Trace

test :: Int -> Gen Int
test i |i < 10 = elements [i] 
 | otherwise  =test i

test2 = sized test
instance Arbitrary  LamTerm where
    arbitrary = resize 100 $sized $ arbitraryTerm []

arbitraryTerm :: [Name] -> Int -> Gen LamTerm
arbitraryTerm [] s = oneof [arbitraryLambda [] s, arbitraryAppl [] s]
arbitraryTerm n s 
    | s /= 0 = frequency [(3,arbitraryLambda n s), (1,arbitraryVar n) ,(1,arbitraryAppl n s)]
    | otherwise = arbitraryVar n

arbitraryLambda :: [Name] -> Int -> Gen LamTerm
arbitraryLambda names s =do
  boolNewName <- case names of 
        [] -> return True 
        otherwise -> return True--frequency [(4,return True),(1,return False)]
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
