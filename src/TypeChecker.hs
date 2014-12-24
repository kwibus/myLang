module TypeChecker where

import BruijnTerm
import Vallue
import Type

import qualified Data.IntMap as IM

typeCheck :: BruijnTerm -> Maybe Type
typeCheck t = go IM.empty t
    where go _ (BVal v) = Return $ typeVal v
          go _ (BLambda _ t) = fmap 


typeVal :: Vallue -> Type
typeVal MyDouble {} = SimpleType TDouble
typeVal BuildIn { myType = t } = t
