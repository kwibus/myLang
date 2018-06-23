{-# LANGUAGE LambdaCase #-}
module TopDown where

import LambdaF
import BruijnEnvironment
import BruijnTerm as Lam hiding(Lambda,Appl,Let)
import MTable(MTable)
import Inprocess

topDownTrans :: (context -> LamTermF () () Bound InProcess -> LamTermF () () Bound InProcess ) -> context -> MTable -> InProcess -> LamTerm () () Bound
topDownTrans f context modifications = unfold go (context, modifications)
  where
    -- go :: context -> MTable -> LamTermF () Bound InProcess -> (LamTermF () Bound (LamTermF () Bound InProcess),(context,MTable))
    go (context_, mod_) ast = (f context_ astF,(newContext,newMod))
      where
        newContext = context --TODO updateContext ast
        (astF,newMod) = peek mod_ ast

topDownTransM :: Monad m => (context -> LamTermF () () Bound InProcess -> m (LamTermF () () Bound InProcess)) -> context -> MTable -> InProcess -> m (LamTerm () () Bound)
topDownTransM f context modifications = unfoldM go (context, modifications)
  where
    -- go :: context -> MTable -> LamTermF () Bound InProcess -> (LamTermF () Bound (LamTermF () Bound InProcess),(context,MTable))
    go (context_, mod_) ast = do
      result <- f context_ astF
      return (result,(newContext,newMod))
      where
        newContext = context --TODO updateContext ast
        (astF,newMod) = peek mod_ ast

