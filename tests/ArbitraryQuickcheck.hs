{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances #-}
module ArbitraryQuickcheck
     where
import Prelude hiding (all,any)
import Data.Foldable hiding (toList)
import Data.Maybe
import Control.Monad.Logic
import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.Applicative

import Control.Exception.Base

import Logic
import GenState
import ArbitraryVallue

import TypeCheck
import Vallue
import BruijnTerm
import Lambda
import Enviroment
import Type


instance Arbitrary (LamTerm Vallue Name ) where
    arbitrary = fmap bruijn2Lam arbitrary

instance Arbitrary (BruijnTerm Vallue ) where
    -- TODO something saver then 10000
    arbitrary = sized $ \ s -> fmap fromJust $ myArbitraryTerm s (TVar (Free 10000))
    shrink = shrinkBruijn

shrinkBruijn :: BruijnTerm Vallue -> [BruijnTerm Vallue ]
shrinkBruijn (Appl t1 t2) = [t1, t2] ++
                             [Appl t1' t2' | (t1', t2') <- shrink (t1, t2)]
shrinkBruijn (Lambda n t) = fastShrink t ++
                             eliminated ++
                             fmap (Lambda n) (shrinkBruijn t)
    where fastShrink (Var {}) = []
          fastShrink _ = [Val (MyDouble 1.0)]
          eliminated = maybeToList $ eliminatedLambda 0 t
shrinkBruijn _ = []

eliminatedLambda :: Index -> BruijnTerm Vallue -> Maybe (BruijnTerm Vallue)
eliminatedLambda i1 (Var (Bound i2))
    | i1 == i2 = Nothing
    | i2 > i1 = Just $ Var $ Bound (i2 - 1)
    | otherwise = Just $ Var $ Bound i2
eliminatedLambda _ (t@Val {}) = Just t
eliminatedLambda i (Lambda n t) = Lambda n <$> eliminatedLambda (i - 1) t
eliminatedLambda i (Appl t1 t2) = Appl <$> eliminatedLambda i t1 <*> eliminatedLambda i t2

myArbitraryTerm :: Int -> Type Free -> Gen(Maybe (BruijnTerm Vallue))
myArbitraryTerm n t =
    fmap listToMaybe $ observeManyT 1 $ fmap snd $ arbitraryTerm n t [] fEmtyEnv defualtGenState


arbitraryTerm :: Int -> Type Free ->[Type Free] -> FreeEnv (Type Free) -> GenState ->
    LogicT Gen (FreeEnv (Type Free), BruijnTerm Vallue)
arbitraryTerm n t maxlist env s
  | n <= 1 = oneOfLogic [ arbitraryVallue t env s
                       , arbitraryVar t env s
                       ]
    -- shorter and parmiterzerd size
  | otherwise = if any(\tt -> tSize tt > 10) (map (`apply` env) maxlist) || 10 < tSize (apply t env) 
        then mzero
        else oneOfLogic [ arbitraryAppl n t maxlist env s
                        , arbitraryLambda n t maxlist env s
                        ]

arbitraryVar :: Type Free -> FreeEnv (Type Free) -> GenState ->
    LogicT Gen (FreeEnv (Type Free), BruijnTerm Vallue)
arbitraryVar t env s = do
  (i, (_, f)) <- elementsLogic $ toList $ dictionary s
  (env2 , expr) <- case  (unify t (TVar f )env ) of
        Right env' -> return ( env' ,Var (Bound ( bruiDepth (dictionary s) - i - 1 )))
        Left _ -> mzero
  assert ( (bruiDepth (dictionary s) - i - 1 ) >= 0)
   assert (snd (bLookup (Bound (bruiDepth (dictionary s) - i - 1 )) (dictionary s)) == f)
   assert (check expr t env2 (fmap snd (dictionary s )))
   return (env2 , expr)

arbitraryAppl :: Int -> Type Free -> [Type Free]-> FreeEnv (Type Free) -> GenState ->
    LogicT Gen (FreeEnv (Type Free), BruijnTerm Vallue)
arbitraryAppl size t maxlist env state = do
  sizeLeft <- chooseLogic (1, size - 1)
  let (newEnv, var) = newFreeVar $ env
  let sizeRight = size - sizeLeft
  (env3, expr) <- if sizeLeft < sizeRight
    then do
      (env1, expr1) <- arbitraryTerm sizeLeft (TAppl (TVar (var)) t)
                                    (TVar var : maxlist) newEnv state
      (env3, expr2) <- arbitraryTerm sizeRight (TVar var)
                                    maxlist env1 state
      return (env3, Appl expr1 expr2)
    else do
      (env1, expr2) <- arbitraryTerm sizeRight (TVar var)
                                    ((TAppl (TVar (var)) t) : maxlist)
                                    newEnv state
      (env3, expr1) <- arbitraryTerm sizeLeft (TAppl (TVar (var)) t)
                                    maxlist env1 state
      return (env3, Appl expr1 expr2)
  assert (check expr t env3 (fmap snd (dictionary state )))
   return (env3, expr)

arbitraryLambda :: Int -> Type Free ->[Type Free] -> FreeEnv (Type Free) -> GenState ->
    LogicT Gen (FreeEnv (Type Free), BruijnTerm Vallue)
arbitraryLambda size t maxlist env (state@State { dictionary = dic}) = do
  let (env1, var1) = newFreeVar $ env
  let (env2, var2) = newFreeVar $ env1
  case unify t (TAppl (TVar var1) (TVar var2)) env2 of
    Left {} -> mzero
    Right env4 -> do
      (n, newState) <- newVarName state
      let (newdic, _) = bInsert (n, var1) dic
      let newnewstate = newState { dictionary = newdic}
      (newEnv, expr) <- arbitraryTerm (size - 1) (TVar var2 ) maxlist env4 newnewstate
      assert (check expr (TVar var2) newEnv (fmap snd newdic))
       assert (check (Lambda n expr) t newEnv (fmap snd dic )) -- explain
       return $ (newEnv, Lambda n expr)

check :: BruijnTerm Vallue -> Type Free -> FreeEnv (Type Free) ->
    BruiEnv Free -> Bool
check expr t1 env dic = case solveWith expr env dic of
  Right (t2, env2) -> case unify (apply t1 env) (apply t2 env2) fEmtyEnv of
       Left _ -> False
       Right {} -> True
  Left _ -> False

newVarName :: GenState -> LogicT Gen (String, GenState)
newVarName state = do
  let names = toList $ dictionary state
  boolNewName <- case names of
        [] -> return True
        _ -> lift $ frequency [(4, return True), (1, return False)]
  newname <- if boolNewName
            then fmap (: []) $ lift $ choose ('a', 'z')
            else lift $ elements $ map (fst . snd) names
  return (newname, state )
