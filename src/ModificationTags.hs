{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module ModificationTags
    ( Modify(..)
    , Ref  (..)
    , LamTerm (..)
    , proces
    , rember
    , piek
    , Def (..)
    )
where

import Control.Monad.Reader
import BruijnEnvironment
import qualified TaggedLambda as Tag
import qualified Lambda as Lam
import BruijnTerm
import Name
import Value

data Modify i = Reorder [Bound] -- ^ @[1, 0, 2]@ will make bound 0 -> 1, 1 -> 0 2 -> 2
              | Substitut (Lam.LamTerm i Bound) -- ^ wil Substitut Bound 0 term. If higher index if deeper
              deriving (Eq, Show)

data LamTerm i n a = Lambda i Name a
            | Appl a a
            | Var i n
            | Val i Value
            | Let i [Def i n a] a
            deriving (Eq, Show)

instance Functor (LamTerm i n) where
    fmap f (Lambda i n t) = Lambda i n $ f t
    fmap f (Appl t1 t2)  = Appl (f t1) (f t2)
    fmap _ (Var i n) = Var i n
    fmap _ (Val i v)  = Val i v
    fmap f (Let i defs t ) = Let i (map fmapDef defs) $ f t
      where
        fmapDef (Def  i_ n_ t_)  = Def i_ n_ $ f t_

data Def i n a = Def i Name a deriving (Eq, Show)

data Ref i = Subst (Lam.LamTerm i Bound)  | Keep Int deriving (Show, Eq)

rember :: Modify i -> BruijnEnv (Ref i) -> BruijnEnv (Ref i)
rember (Reorder order) env = bReorder env order
rember (Substitut term) env = bInsert (Subst term) env

data Unprocessed i = Unprocessed Int (Tag.LamTerm i Bound (Modify i))
                deriving Show

-- TODO find better name
piek :: MonadReader (BruijnEnv (Ref i)) m => Unprocessed i -> (LamTerm i Bound (Unprocessed i) -> m a)  -> m a
piek (Unprocessed depth term) f = case term of
    Tag.Tag m t -> local (rember m)( piek (Unprocessed depth t) f)
    Tag.Val i v -> f (Val i v)
    Tag.Var i b@(Bound n) -> do
        env <- ask
        case bMaybeLookup b env of
            Just (Keep depthDefined) -> f (Var i $ Bound $ depth - depthDefined - 1)
            Just (Subst t2) -> let increase = depth - nsubst (bDrop (n+1) env)
                               in local (const bEmtyEnv) $ piek (Unprocessed 0 $ Tag.tag $ incFree increase t2) f -- TODO this can faster?
            Nothing -> f ( Var i (Bound $ n - nsubst env))

    Tag.Appl t1 t2 -> f (Appl (Unprocessed depth t1) (Unprocessed depth t2))
    Tag.Lambda i n t ->
        local (bInsert $ Keep depth) $ f (Lambda i n $ Unprocessed (depth +1) t)
    Tag.Let i defs t ->
        local (bInserts $ map Keep [depth .. newDepth -1]) $
        f (Let i (map piekDef defs ) (Unprocessed newDepth t ))
          where
            nDefs = length defs
            newDepth = depth + nDefs
            piekDef (Tag.Def i_ n_ t_) = Def i_ n_ $ Unprocessed newDepth t_

nsubst::  BruijnEnv (Ref i) -> Int
nsubst = bSize . bFilter (\case
                 Subst {}-> True
                 _ -> False  )

--TODO add comments
proces :: Tag.LamTerm () Bound (Modify ())-> Lam.LamTerm () Bound
proces term =
  -- go term 0 bEmtyEnv
  runReader (go $ Unprocessed 0 term) bEmtyEnv
  where
    go unprocced = piek unprocced $ \case
            Var i b -> return $ Lam.Var i b
            Val i v -> return $ Lam.Val i v

            Appl t1 t2 -> do
                t1' <-go t1
                t2' <- go t2
                return $ Lam.Appl t1' t2'
            Lambda i n t -> Lam.Lambda i n <$> go t
            Let i defs t -> Lam.Let i <$> mapM goDef defs <*> go t
              where
                goDef (Def i_ n_ t_) = Lam.Def i_ n_ <$> go t_

-- TODO remove duplcated incfree (also in eval)
incFree :: Int -> BruijnTerm i -> BruijnTerm i
incFree 0 term = term
incFree increase  term = go 0 term
  where
    go :: Int -> BruijnTerm i -> BruijnTerm i
    go depth (Lam.Lambda i n t) =Lam.Lambda i n $ go (depth+1) t
    go depth (Lam.Appl t1 t2) = Lam.Appl (go depth t1)(go depth t2)
    go depth (Lam.Var i (Bound n)) | n >= depth  = Lam.Var i $ Bound $ n+increase
                               | otherwise = Lam.Var i (Bound n)
    go depth (Lam.Let i defs t) = Lam.Let i (fmap incDefs defs) $ go newDepth t
      where
        newDepth = depth +length defs
        incDefs (Lam.Def is ns ts) = Lam.Def is ns $ go newDepth ts
    go _ (Lam.Val i v) = Lam.Val i v
