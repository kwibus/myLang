module SmallCheck where

import GenState
import Test.SmallCheck.Series
import ArbiRef
import ArbitraryLambda
import Type
import Lambda
import Test.QuickCheck.Gen (Gen, unGen)
import Test.QuickCheck.Random (mkQCGen)

-- TODO now smallcheck can be slow because of uneeded shuffeling (quickckeck)
-- but terms get already to pig for real test

serieTerm :: ArbiRef n => Maybe Type -> Series m (LamTerm () n)
serieTerm t = generateG $ \ d -> generateList $ generateTerm d t

generateG :: (Int -> Gen [a]) -> Series m a
generateG f = generate (\ d -> unsafeGenerate (f d) d)

unsafeGenerate :: Gen [a] -> Int -> [a]
unsafeGenerate gen = unGen gen (mkQCGen 0)
