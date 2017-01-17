{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Modify where

import Control.Monad.State.Lazy

import Unprocessed
import BruijnTerm as Lam
import BruijnEnvironment
import LambdaF
import qualified TaggedLambda as Tag
import ModificationTags

-- TODO split file
applyModify :: Tag.LamTerm i Bound (Modify i ) -> Lam.LamTerm i Bound
applyModify term = evalState (proces (Unprocessed term)) empty

sub :: BruijnTerm i -> Unprocessed i -> Unprocessed i
sub = addTag . Substitut

reorder :: [Int] -> Unprocessed i -> Unprocessed i
reorder = addTag . Reorder . map Bound

-- TODO rename remove use reader
localT :: MonadState (SymbolTable i) m => m a -> m a
localT m = do
    oldSymbolTable <- get
    a <- m
    put oldSymbolTable
    return a

data Symbol i = Subst Int (BruijnTerm i)
           | Undefined Int
           | Keep Int Int (BruijnTerm i)
           deriving (Show, Eq)

remember :: Modify i -> SymbolTable i -> SymbolTable i
remember modification s@SymbolTable {getEnv = env} = remember' modification
  where
    remember' (Reorder order)  = s {getEnv = bReorder env order}
    remember' (Substitut term) = s {getEnv = bInsert (Subst (bruijnDepth env + 1) term) env}

-- rename
insertT :: [Symbol i] -> SymbolTable i -> SymbolTable i
insertT refs s@SymbolTable {getEnv = env, getDepth = depth}
    = s {getEnv = bInserts refs env
        , getDepth = depth + length refs}

-- TODO remove
getLambdaT :: Unprocessed i -> Tag.LamTerm i Bound (Modify i)
getLambdaT (Unprocessed l) = l

-- TODO env rename name , get simbolTabe
data SymbolTable i = SymbolTable
            { getDepth :: Int
            , getEnv :: BruijnEnv (Symbol i)}
              deriving (Eq, Show)

empty :: SymbolTable i
empty = SymbolTable 0 bEmtyEnv

-- | peek allows you to see the modified tree, without modifying hole tree
--   it uses a state monad to remember witch modifications it still have to to
-- TODO find better name
-- peak
peek :: MonadState (SymbolTable i) m
     => Unprocessed i
    -> (LamTermF i Bound (Unprocessed i) -> m a)
    -> m a
peek (Unprocessed term) f = case term of
    Tag.Tag m t -> localT $ modify (remember m) >> peek (Unprocessed t) f
    Tag.Val i v -> f (ValF i v)
    Tag.Var i b@(Bound n) -> do
        env <- gets getEnv
        depth <- gets getDepth
        case bMaybeLookup b env of
            Just (Keep depthDefined orignalsize foundTerm) ->
                let varname = Bound $ depth - depthDefined - 1
                    newTagTerm = Tag.tag $ incFree (bruijnDepth env - orignalsize) foundTerm
                in f ( PtrF i varname $ Unprocessed newTagTerm)

            Just (Undefined depthDefined) -> f (VarF i $ Bound $ depth - depthDefined - 1)
            Just (Subst orignalsize t2) ->
                    -- TODO this can faster?
                let newTagTerm = Unprocessed $ Tag.tag $ incFree (bruijnDepth env - orignalsize + 1) t2
                in peek newTagTerm f
            Nothing -> f ( VarF i (Bound $ n - nsubst env)) --TODO nsubst can be memorize wordt it ?

    Tag.Appl t1 t2 -> f (ApplF (Unprocessed t1) (Unprocessed t2))
    Tag.Lambda i n t -> do
        depth <- gets getDepth
        localT $ modify (insertT [Undefined depth] ) >> f (LambdaF i n $ Unprocessed t)
    Tag.Let i defs t -> do
        depth <- gets getDepth
        let newDepth = depth + nDefs
        localT ( do
                modify $ insertT $ map Undefined [depth .. newDepth - 1]
                f (LetF i (map peekDef defs ) (Unprocessed t ))
                 )
          where
            nDefs = length defs
            peekDef (Tag.Def i_ n_ t_) = DefF i_ n_ $ Unprocessed t_

substitut :: MonadState (SymbolTable i) m => Bound -> BruijnTerm i -> m ()
substitut b term = modify ( \ (SymbolTable depth env) -> SymbolTable (depth - 1) $
            bReplace b (Subst (bruijnDepth env) term) env )

-- TODO rename inline
storeT :: Bound -> BruijnTerm i -> SymbolTable i -> SymbolTable i
storeT b@(Bound offset) term (SymbolTable depth env) = SymbolTable depth $ bReplace b
    (Keep (depth - offset - 1) (bruijnDepth env) term)
    env

store :: MonadState (SymbolTable i) m => Bound -> BruijnTerm i -> m ()
store b term = modify (storeT b term)

nsubst :: BruijnEnv (Symbol i) -> Int
nsubst = bSize . bFilter (\ case
                 Subst {} -> True
                 _ -> False)

proces :: MonadState (SymbolTable i) m => Unprocessed i -> m (Lam.LamTerm i Bound)
proces unprocessed = localT $ peek unprocessed $ \ case
            PtrF i b _ -> return $ Lam.Var i b
            VarF i b -> return $ Lam.Var i b
            ValF i v -> return $ Lam.Val i v
            ApplF t1 t2 -> do
                t1' <- proces t1
                t2' <- proces t2
                return $ Lam.Appl t1' t2'
            LambdaF i n t -> Lam.Lambda i n <$> proces t
            LetF i defs t -> Lam.Let i <$> mapM procesDef defs <*> proces t

procesDef :: MonadState (SymbolTable i) m => DefF i Bound (Unprocessed i) -> m (Lam.Def i Bound)
procesDef (DefF i n t) = Lam.Def i n <$> proces t
incFree :: Int -> BruijnTerm i -> BruijnTerm i
incFree = incFreeOfset 0

incFreeOfset :: Int -> Int -> BruijnTerm i -> BruijnTerm i
incFreeOfset _ 0 term = term

incFreeOfset ofset increase term = go ofset term
  where
    go :: Int -> BruijnTerm i -> BruijnTerm i
    go depth (Lam.Lambda i n t) = Lam.Lambda i n $ go (depth + 1) t
    go depth (Lam.Appl t1 t2) = Lam.Appl (go depth t1) (go depth t2)
    go depth (Lam.Var i (Bound n))
        | n >= depth = Lam.Var i $ Bound $ n + increase
        | otherwise = Lam.Var i (Bound n)
    go depth (Lam.Let i defs t) = Lam.Let i (fmap incDefs defs) $ go newDepth t
      where
        newDepth = depth + length defs
        incDefs (Lam.Def is ns ts) = Lam.Def is ns $ go newDepth ts
    go _ (Lam.Val i v) = Lam.Val i v
