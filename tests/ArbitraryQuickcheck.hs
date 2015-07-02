{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances #-}
module ArbitraryQuickcheck
     where
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Test.QuickCheck.Gen
import Test.QuickCheck

import Logic
import GenState
import ArbitraryVallue
import MakeTerm
import BruijnTerm
import Lambda
import Enviroment
import Type
import Names
import ArbiRef

instance Arbitrary (LamTerm () Name ) where
    arbitrary = sized $ \ s -> fromJust <$> myArbitraryTerm s (TVar (Free 10000)) -- remove dup

instance Arbitrary (BruijnTerm ()) where
    -- TODO something saver then 10000
  arbitrary = sized $ \ s -> fromJust <$> myArbitraryTerm s (TVar (Free 10000))
  shrink = shrinkBruijn

shrinkBruijn :: BruijnTerm () -> [BruijnTerm ()]
shrinkBruijn (Appl _ t1 t2) = [t1, t2] ++
                             [Appl () t1' t2' | (t1', t2') <- shrink (t1, t2)]
shrinkBruijn (Lambda _ (Name n) t) = fastShrink t ++
                             eliminated ++
                             (lambda n <$> shrinkBruijn t)
    where fastShrink (Val _ v) = val <$> shrinkValue v
          fastShrink _ = []
          eliminated = maybeToList $ eliminatedLambda 0 t
shrinkBruijn _ = []

eliminatedLambda :: Int -> BruijnTerm () -> Maybe (BruijnTerm ())
eliminatedLambda i1 (Var () (Bound i2))
    | i1 == i2 = Nothing
    | i1 < i2 = Just $ Var () $ Bound (i2 - 1)
    | otherwise = Just $ Var () $ Bound i2
eliminatedLambda _ (t@Val {}) = Just t
eliminatedLambda i (Lambda _ n t) = Lambda () n <$> eliminatedLambda (i + 1) t
eliminatedLambda i (Appl _ t1 t2) =
    Appl () <$> eliminatedLambda i t1 <*> eliminatedLambda i t2

myArbitraryTerm :: ArbiRef n => Int -> Type Free -> Gen ( Maybe (LamTerm () n ))
myArbitraryTerm n t = runGenerartor $ arbitraryTerm n t [] defualtGenState

arbitraryTerm :: ArbiRef n => Int -> Type Free -> [Type Free] -> GenState n -> Generater (LamTerm () n)
arbitraryTerm n t maxlist s
  | n <= 1 = oneOfLogic [ arbitraryVallue t
                        , arbitraryVar t s
                        ]
    -- shorter and parmiterzerd size
  | otherwise = do
    b1 <- or <$> mapM (typesizeSmaller 7 ) maxlist
    b2 <- typesizeSmaller 7 t
    if b1 || b2
        then mzero
        else oneOfLogic [ arbitraryAppl n t maxlist s
                        , arbitraryLambda n t maxlist s
                        ]

arbitraryVar :: ArbiRef n => Type Free -> GenState n -> Generater (LamTerm () n)
arbitraryVar t s = do
  (n, f) <- refFromState s
  unifyGen t (TVar f)
  return $ Var () n

arbitraryAppl :: ArbiRef n => Int -> Type Free -> [Type Free] -> GenState n -> Generater (LamTerm () n)
arbitraryAppl size t maxlist state = do
  sizeLeft <- chooseLogic (1, size - 1)
  newvar <- newFreeVar
  let sizeRight = size - sizeLeft
  if sizeLeft < sizeRight
  then do
          expr1 <- arbitraryTerm sizeLeft (TAppl (TVar newvar) t) (TVar newvar : maxlist) state
          expr2 <- arbitraryTerm sizeRight (TVar newvar) maxlist state
          return $ appl expr1 expr2
  else do
          expr2 <- arbitraryTerm sizeRight (TVar newvar) (TAppl (TVar newvar) t : maxlist) state
          expr1 <- arbitraryTerm sizeLeft (TAppl (TVar newvar) t) maxlist state
          return $ appl expr1 expr2

arbitraryLambda :: ArbiRef n => Int -> Type Free -> [Type Free] -> GenState n ->
    Generater ( LamTerm () n)
arbitraryLambda size t maxlist state = do
  var1 <- newFreeVar
  var2 <- newFreeVar
  unifyGen t $ TAppl (TVar var1) (TVar var2)
  (n, newState) <- lift $ newVarRef state var1
  expr <- arbitraryTerm (size - 1) (TVar var2 ) maxlist newState
  return $ lambda n expr

newVarRef :: ArbiRef n => GenState n -> Free -> Gen (String, GenState n)
newVarRef state free = do
  let names = toList $ dictionary state
  boolNewName <- case names of
        [] -> return True
        _ -> frequency [(4, return True), (1, return False)]
  newname <- if boolNewName
            then (: []) <$> choose ('a', 'z')
            else elements $ map (fst . snd) names
  return (newname, updateState state boolNewName newname free)

-- check :: BruijnTerm () -> Type Free -> BruiEnv (Free )-> Generater Bool
-- check expr t1 dic = do
--  env <- getEnv
--  return $ case runInfer (solveWith expr env dic) of
--     Right (t2, env2) -> case unify (apply t1 env) (apply t2 env2) fEmtyEnv of
--        Left _ -> False
--        Right {} -> True
--     Left _ -> False
