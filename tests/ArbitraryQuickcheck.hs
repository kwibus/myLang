{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances #-}
module ArbitraryQuickcheck
     where
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.Applicative

import Logic
import GenState
import ArbitraryVallue
import MakeTerm
import TypeCheck hiding (newFreeVar)
import Vallue
import BruijnTerm
import Lambda
import Enviroment
import Type
import Names

instance Arbitrary (LamTerm () Name ) where
    arbitrary = fmap bruijn2Lam arbitrary

instance Arbitrary (BruijnTerm ()) where
    -- TODO something saver then 10000
  arbitrary = sized $ \ s -> fmap fromJust $
        myArbitraryTerm s (TVar (Free 10000))
  -- shrink = shrinkBruijn

shrinkBruijn :: BruijnTerm () -> [BruijnTerm ()]
shrinkBruijn (Appl _ t1 t2) = [t1, t2] ++
                             [Appl () t1' t2' | (t1', t2') <- shrink (t1, t2)]
shrinkBruijn (Lambda _ n t) = fastShrink t ++
                             eliminated ++
                             fmap (Lambda () n) (shrinkBruijn t)
    where fastShrink (Var {}) = []
          fastShrink _ = [Val () (MyDouble 1.0)]
          eliminated = maybeToList $ eliminatedLambda 0 t
shrinkBruijn _ = []

eliminatedLambda :: Int -> BruijnTerm () -> Maybe (BruijnTerm ())
eliminatedLambda i1 (Var () (Bound i2))
    | i1 == i2 = Nothing
    | i2 > i1 = Just $ Var () $ Bound (i2 - 1)
    | otherwise = Just $ Var () $ Bound i2
eliminatedLambda _ (t@Val {}) = Just t
eliminatedLambda i (Lambda _ n t) = Lambda () n <$> eliminatedLambda (i - 1) t
eliminatedLambda i (Appl _ t1 t2) =
    Appl () <$> eliminatedLambda i t1 <*> eliminatedLambda i t2


myArbitraryTerm :: Int -> Type Free -> Gen ( Maybe (BruijnTerm ()))
myArbitraryTerm n t = runGenerartor $ arbitraryTerm n t [] defualtGenState

arbitraryTerm :: Int -> Type Free -> [Type Free] -> GenState ->  Generater (BruijnTerm ())
arbitraryTerm n t maxlist s
  | n <= 1 = oneOfLogic [ arbitraryVallue t
                       , arbitraryVar t s
                       ]
    -- shorter and parmiterzerd size
  | otherwise = do 
    b1 <- fmap (any id)$ mapM  (typesizeSmaller 10)maxlist 
    b2 <- typesizeSmaller 10  t
    if  b1 || b2 
        then mzero
        else oneOfLogic [ arbitraryAppl n t maxlist s
                        , 
         arbitraryLambda n t maxlist s
                        ]

arbitraryVar :: Type Free -> GenState -> Generater (BruijnTerm ())
arbitraryVar t s = do
  (i, (_,((), f))) <- elementsLogic $ toList $ dictionary s
  unifyGen t (TVar f)
  return $ bvar (bruiDepth (dictionary s ) -i -1)

arbitraryAppl :: Int -> Type Free -> [Type Free] -> GenState -> Generater (BruijnTerm ())
arbitraryAppl size t maxlist state = do

  sizeLeft <- chooseLogic (1, size - 1)
  newvar <- newFreeVar 
  let sizeRight = size - sizeLeft
  if sizeLeft < sizeRight
  then do
          expr1 <- arbitraryTerm sizeLeft (TAppl (TVar (newvar)) t) (TVar newvar : maxlist) state
          expr2 <- arbitraryTerm sizeRight (TVar newvar) maxlist state
          return $ appl expr1 expr2
  else do
          expr2 <- arbitraryTerm sizeRight (TVar newvar) ((TAppl (TVar (newvar)) t) : maxlist) state
          expr1 <- arbitraryTerm sizeLeft (TAppl (TVar (newvar)) t) maxlist state
          return $ appl expr1 expr2

arbitraryLambda :: Int -> Type Free -> [Type Free] -> GenState ->
    Generater ( BruijnTerm ())
arbitraryLambda size t maxlist (state@State { dictionary = dic}) = do
  var1 <- newFreeVar
  var2 <-  newFreeVar
  unifyGen t $TAppl (TVar var1) (TVar var2)
  (n, newState) <- newVarName state
  let (newdic, _) = bInsert (n, ((),var1)) dic
  let newnewstate = newState { dictionary = newdic}
  expr <- arbitraryTerm (size - 1) (TVar var2 ) maxlist newnewstate
  return $ ( lambda n expr)

check :: BruijnTerm () -> Type Free -> BruiEnv (Free )-> Generater Bool
check expr t1 dic = do 
 env <- getEnv
 return $ case runInfer (solveWith expr env dic) of
    Right (t2, env2) -> case unify (apply t1 env) (apply t2 env2) fEmtyEnv of
       Left _ -> False
       Right {} -> True
    Left _ -> False

newVarName :: GenState -> Generater(String, GenState)
newVarName state = do
  let names = toList $ dictionary state
  boolNewName <- case names of
        [] -> return True
        _ -> lift $frequency [(4, return True), (1, return False)]
  newname <- if boolNewName
            then fmap (: []) $ lift $ choose ('a', 'z')
            else lift $ elements $ map (fst . snd) names
  return (newname, state )
