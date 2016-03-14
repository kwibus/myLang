{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances #-}
module ArbitraryType (arbitraryType) where

import Test.QuickCheck.Gen
import Test.QuickCheck
import Type
import MakeType

instance Arbitrary Type where
    arbitrary = sized arbitraryType

-- TODO Beter Arbitrary type  possible ?
arbitraryType :: Int -> Gen Type
arbitraryType s | s <= 1 = oneof [fmap TVal arbitraryMonoType, arbitraryTVar]
                | otherwise =
  do
     sizeleft <- choose (1, s - 1)
     t1 <- arbitraryType sizeleft
     t2 <- arbitraryType (s - sizeleft )
     return $ TAppl t1 t2

 -- TODO rename. is not a monot typt but primitive or TypeInstance
arbitraryMonoType :: Gen TypeInstance
arbitraryMonoType = elements [TDouble, TBool]

arbitraryTVar :: Gen Type
arbitraryTVar = do
    d <- choose (0, 10)
    return $ tVar d
