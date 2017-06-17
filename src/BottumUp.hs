{-# LANGUAGE LambdaCase #-}
module BottumUp where

import qualified TaggedLambda as Tag
import LambdaF
import BruijnEnvironment
import BruijnTerm
import ModificationTags

data Result = NoChange -- replace Maybe
            | Changed InProcess  --TODO capital P
            -- TODO Maybe at Stop

data InProcess = Inproc (LamTermF () Bound InProcess)
              | Unproc (Tag.LamTerm () Bound (Modify()))
              | New (LamTerm () Bound)
              deriving Show

inc :: Int -> MTable -> MTable
inc n t@MTable{} = t{getDepth = getDepth t + n}

remember :: Modify () -> MTable -> MTable
remember modification s@MTable {getEnv = env} = remember' modification
  where
    remember' (Reorder n order)  = s {getEnv = bReorder env n order} --TODO
    remember' (SubstitutT n term) = remember  (Substitut n $ proces s $ Unproc term) s
    remember' (Substitut n term) = s {getEnv = bInsertAt n (Subst (getDepth s) term) env}

-- TODO env rename name , get simbolTabe
data MTable = MTable
            { getDepth :: Int
            , getEnv :: BruijnEnv (Symbol ())}
              deriving (Eq, Show)

empty :: MTable
empty = MTable 0 bEmtyEnv

data Symbol i = Subst Int (BruijnTerm i)
           | Undefined Int
           deriving (Show, Eq)

-- unFT :: LamTermF i n (Tag.LamTerm i n t) -> Tag.LamTerm i n t
-- unFT (VarF i n) = Tag.Var i n
-- unFT (ApplF t1 t2) = Tag.Appl t1 t2
-- unFT (ValF i v) = Tag.Val i v
-- unFT (LambdaF i n t) = Tag.Lambda i n t
-- -- unFT (VarF i n) = Tag.Var i n
--

free :: LamTerm i n -> LamTermF i n (LamTerm i n)
free (Var i n) = VarF i n
free (Appl t1 t2) = ApplF t1 t2
free (Val i v) = ValF i v
free (Lambda i n t) = LambdaF i n t
-- unFT (VarF i n) = Tag.Var i n

nsubst :: BruijnEnv (Symbol i) -> Int
nsubst = bSize . bFilter (\ case
                 Subst {} -> True
                 _ -> False)

-- rename
insertT :: [Symbol ()] -> MTable -> MTable
insertT refs s@MTable {getEnv = env, getDepth = depth}
    = s {getEnv = bInserts refs env
        , getDepth = depth + length refs}

-- TODO rename mod
peek :: MTable -> InProcess -> (LamTermF () Bound InProcess, MTable)
peek modification (Inproc term)= (term,modification) --FIXME
peek mod (New term) = (New <$> free term,mod)
peek mod (Unproc term) = case term of
    Tag.Tag m t -> peek (remember m mod ) (Unproc t)
    Tag.Val i v -> (ValF i v,mod)
    Tag.Var i b@(Bound n) ->
      let env = getEnv mod
      in case bMaybeLookup b (getEnv mod) of
            Just (Undefined depthDefined) -> (VarF i $ Bound $ getDepth mod - depthDefined - 1,mod)
            Just (Subst orignalsize t2) ->
                    -- TODO this can faster? drop part env instead of increase free?
                let newTerm = fmap New $free $ incFree (getDepth mod - orignalsize) t2
                in  (newTerm ,mod)
            Nothing -> ( VarF i (Bound $ n - nsubst env),mod) --TODO nsubst can be memorize wordt it ?
    Tag.Appl t1 t2 -> (ApplF (Unproc t1) (Unproc t2),mod)
    Tag.Lambda i n t -> (LambdaF i n $ Unproc t,insertT [Undefined $getDepth mod] mod)
    Tag.Let i defs t -> ( LetF i (map peekDef defs) (Unproc t),insertT (map Undefined [depth .. depth + nDefs-1]) mod )
          where
            nDefs = length defs
            depth = getDepth mod
            peekDef (Tag.Def i_ n_ t_) = DefF i_ n_ $ Unproc t_

-- mapSup :: MTable -> (MTable -> LamTermF () Bound InProcess -> LamTermF () Bound a) -> LamTermF () Bound InProcess  -> LamTermF () Bound (LamTermF () Bound a )
-- mapSup modifications f = fmap (\subAst ->
--     let (subAstF,newmod) = peek modifications subAst
--     in f newmod subAstF)
--
-- merge :: (LamTermF i n a -> LamTermF i n(LamTermF i n a)) -> LamTermF i n a -> LamTerm i n
-- merge f a = case f a of
--     VarF i n -> Var i n
--     ValF i v -> Val i v
--     LambdaF i n t -> Lambda i n (merge f t)
--     ApplF t1 t2  -> Appl (merge f t1) (merge f t2)
--     LetF i defs t -> Let i (map mergeDef defs) (merge f t)
--       where mergeDef (DefF i_ n_ t_) = Def i_ n_ (merge f t_)

--FIXME rename
unfold :: (b -> a -> (LamTermF i n a,b)) -> b -> a -> LamTerm i n
unfold f b a = case f b a of
    (VarF i n,_) -> Var i n
    (ValF i v,_) -> Val i v
    (LambdaF i n t1, b1) -> Lambda i n (unfold f b1 t1)
    (ApplF t1 t2, b1)  -> Appl (unfold f b1 t1) (unfold f b1 t2)
    (LetF i defs t, b1) -> Let i (map unfoldDef defs) (unfold f b1 t)
      where unfoldDef (DefF i_ n_ t_) = Def i_ n_ (unfold f b1 t_)

--FIXME rename
unfoldM :: Monad m => (b -> a -> m (LamTermF i n a,b)) -> b -> a -> m (LamTerm i n)
unfoldM f b a = do
  result <- f b a
  case result of
    (VarF i n,_) -> return $ Var i n
    (ValF i v,_) -> return $ Val i v
    (LambdaF i n t1, b1) -> Lambda i n <$> unfoldM f b1 t1
    (ApplF t1 t2, b1) -> Appl <$> unfoldM f b1 t1 <*> unfoldM f b1 t2
    (LetF i defs t, b1) -> Let i <$> mapM unfoldDefM defs <*> unfoldM f b1 t
      where unfoldDefM (DefF i_ n_ t_) = Def i_ n_ <$> unfoldM f b1 t_

applyModify :: Tag.LamTerm () Bound (Modify ()) -> LamTerm () Bound
applyModify term =  proces empty $ Unproc term

proces ::  MTable  -> InProcess -> LamTerm () Bound
proces = unfold peek

procesDef :: MTable-> DefF () Bound InProcess -> Def () Bound
procesDef modifications (DefF i n t) = Def i n (proces modifications t)

topDownTrans :: (context -> MTable-> LamTermF () Bound InProcess -> Result ) -> context -> MTable -> InProcess -> LamTerm () Bound
topDownTrans f context modifications = unfold go (context, modifications)
  where
    -- go :: context -> MTable -> LamTermF () Bound InProcess -> (LamTermF () Bound (LamTermF () Bound InProcess),(context,MTable))
    go (context_, mod_) ast = case f context_ mod_ astF of
      NoChange -> (astF,(newContext,newMod))
      Changed uf -> go (context, mod_) uf
      where
        newContext = context --TODO updateContext ast
        (astF,newMod) = peek mod_ ast

topDownTransM :: Monad m => (context -> MTable-> LamTermF () Bound InProcess ->m Result ) -> context -> MTable -> InProcess -> m (LamTerm () Bound)
topDownTransM f context modifications = unfoldM go (context, modifications)
  where
    -- go :: context -> MTable -> LamTermF () Bound InProcess -> (LamTermF () Bound (LamTermF () Bound InProcess),(context,MTable))
    go (context_, mod_) ast = do
      result <- f context_ mod_ astF
      case result of
        NoChange -> return (astF,(newContext,newMod))
        Changed uf -> go (context, mod_) uf
      where
        newContext = context --TODO updateContext ast
        (astF,newMod) = peek mod_ ast

deepin :: Tag.LamTerm i n (Modify i)  -> LamTermF i n (Tag.LamTerm i n (Modify i))
deepin (Tag.Var i n) = VarF i n
deepin (Tag.Appl t1 t2) = ApplF t1 t2
deepin (Tag.Val i v) = ValF i v
deepin (Tag.Lambda i n t) = LambdaF i n t
deepin (Tag.Tag m t) = (Tag.Tag $ deepinTag(depthChange t) m ) <$> deepin t
  where
    deepinTag  :: Int -> Modify i -> Modify i
    deepinTag n (Reorder d order) = Reorder (d+n) order

    depthChange :: Tag.LamTerm i n (Modify i)  -> Int
    depthChange (Tag.Tag m t) = depthChange t --could be faster
    depthChange Tag.Lambda {} = 1
    depthChange (Tag.Let _ defs _) = length defs
    depthChange _ = 0

incFree :: Int -> BruijnTerm i -> BruijnTerm i
incFree = incFreeOfset 0

incFreeOfset :: Int -> Int -> BruijnTerm i -> BruijnTerm i
incFreeOfset _ 0 term = term

incFreeOfset ofset increase term = go ofset term
  where
    go :: Int -> BruijnTerm i -> BruijnTerm i
    go depth (Lambda i n t) = Lambda i n $ go (depth + 1) t
    go depth (Appl t1 t2) = Appl (go depth t1) (go depth t2)
    go depth (Var i (Bound n))
        | n >= depth = Var i $ Bound $ n + increase
        | otherwise = Var i (Bound n)
    go depth (Let i defs t) = Let i (fmap incDefs defs) $ go newDepth t
      where
        newDepth = depth + length defs
        incDefs (Def is ns ts) = Def is ns $ go newDepth ts
    go _ (Val i v) = Val i v
