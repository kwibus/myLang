{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances #-}
module ArbitraryType (arbitraryType) where

import Test.QuickCheck.Gen
import Test.QuickCheck

import Type
import Environment

instance Arbitrary (Type Bound ) where
    arbitrary = sized arbitraryType
instance Arbitrary (Type Free) where
    arbitrary = fmap typeBound2Free (arbitrary :: Gen (Type Bound) )

-- TODO Beter Arbitrary type  possible ?
arbitraryType :: Int -> Gen (Type Bound)
arbitraryType s | s <= 1 = oneof [fmap TVal arbitraryMonoType, arbitraryTVar]
                | otherwise =
  do
     sizeleft <- choose (1, s - 1)
     t1 <- arbitraryType sizeleft
     t2 <- arbitraryType (s - sizeleft )
     return $ TAppl t1 t2

arbitraryMonoType :: Gen MonoType
arbitraryMonoType = elements [TDouble]

arbitraryTVar :: Gen (Type Bound)
arbitraryTVar = do
    d <- choose (0, 10)
    TVar <$> elements [Bound d]
