{-# LANGUAGE FlexibleContexts #-}
module BottumUp where

import Control.Monad.Reader

import qualified TaggedLambda as Tag
import LambdaF
import BruijnEnvironment
import BruijnTerm (BruijnTerm)
import ModificationTags
import MTable
import Control.Monad.Trans.Cont

newtype Old = Old (LamTerm ()) deriving Show
type BtUp a = Reader MTable a

procesM :: MonadReader MTable m => Old -> m (BruijnTerm ())
procesM (Old term ) = do
  m <- ask
  return $ proces m term

-- could it make drop read
reproces :: MonadReader MTable m => (Old-> m a) -> LamTerm () -> m a
reproces f term = local (const empty) $ f (Old term)

--TODO could make make result not mondaic?
peekNew :: MonadReader MTable m => LamTerm () -> (LamTermF () Bound Old -> m a) -> m a
peekNew ast f = do
  let (astF,newM) = peek empty ast
  local (const newM) (f $ fmap Old astF)

peekM :: MonadReader MTable m => Old -> (LamTermF () Bound Old -> m a) -> m a
peekM (Old ast) f = do
  m <- ask
  let (astF,newM) = peek m ast
  local (const newM) (f $ fmap Old astF)

peekF :: MonadReader MTable m => Old -> m (LamTermF () Bound ((Old -> m a)->m a))
peekF (Old ast)  = do
  m <- ask
  let (astF,newM) = peek m ast
  return $ fmap (\a f -> local (const newM) $ f (Old a)  )  astF

peekC :: MonadReader MTable m => Old -> m (LamTermF () Bound (ContT r m Old))
peekC (Old ast) = do
  m <- ask
  let (astF,newM) = peek m ast
  return $ fmap (\a-> ContT $ \f -> local (const newM) $ f $ Old a) astF

--TODO rename
trans :: MonadReader MTable m => Old -> (LamTermF () Bound Old -> m (BruijnTerm ())) -> m (BruijnTerm ())
trans = peekM

-- TODO move Old like function to different module
-- TODO should there be a version with new ? for appl,substitute ...
substitute :: BruijnTerm () -> Old -> Old
substitute sub (Old term) =Old $ Tag.Tag (Substitut 0 sub) term

appl :: Old -> Old -> Old
appl (Old t1) (Old t2) = Old (Tag.Appl t1 t2)

bottumUpM :: MonadReader MTable m
          => (LamTermF () Bound (LamTerm ()) -> m a)
          -> (Old -> m (LamTerm ()))
          -> Old
          -> m a

bottumUpM f r (Old ast) = do
  m <- ask
  let (astF,newM) = peek m ast
  newAstF <- traverse (local (const newM).r) (fmap Old astF)
  local (const empty) (f newAstF)


--TODO use Old
bottumUp :: (LamTermF () Bound (LamTerm ()) -> LamTerm ())
          -> LamTerm ()
          -> LamTerm ()
bottumUp f ast = bottumUpWithContext empty ignore (\() -> f) () ast
  where ignore _ _ = ()

bottumUpWithContext :: MTable
          -> (context -> LamTermF () Bound (LamTerm ()) -> context)
          -> (context -> LamTermF () Bound (LamTerm ()) -> LamTerm ())
          -> context
          -> LamTerm ()
          -> LamTerm ()

bottumUpWithContext modifications  updateContext f context ast = f context $ fmap (bottumUpWithContext newModifications updateContext  f newContext) astF
  where
    newContext = updateContext context astF
    (astF,newModifications) = peek modifications ast

