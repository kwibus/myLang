module ArbitraryLambda
where

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Test.QuickCheck
import Test.QuickCheck.Property

import Logic
import GenState
import ArbitraryValue
import MakeTerm
import BruijnTerm
import Lambda
import BruijnEnvironment
import FreeEnvironment
import Type
import Name
import ArbiRef
import PrettyPrint
import Info
import Eval

forAllTypedBruijn :: Testable prop => (BruijnTerm () -> prop) -> Property
forAllTypedBruijn = forAllShowShrink genTyped printBrujin shrinkTypedBruijn

printBrujin :: BruijnTerm i -> String
printBrujin b = let b' = removeInfo b
                in case bruijn2Lam b of
                       Right l -> PrettyPrint.pShow l
                       _  -> show b'

forAllUnTypedLambda :: Testable prop => (LamTerm () Name -> prop) -> Property
forAllUnTypedLambda = forAllShrink genUnTyped shrinkUnTypedLambda

forAllUnTypedBruijn :: Testable prop => (BruijnTerm () -> prop) -> Property
forAllUnTypedBruijn = forAllShrink genUnTyped shrinkUnTypedBruijn

forAllShowShrink :: Testable prop => Gen a -> ( a -> String) -> (a -> [a]) -> (a -> prop) -> Property
forAllShowShrink gen myShow shrinker pf =
  MkProperty $
  gen >>= \x ->
    unProperty $
    shrinking shrinker x $ \x' ->
      counterexample (myShow x') (pf x')

shrinkTypedBruijn :: LamTerm () Bound -> [LamTerm () Bound]
shrinkTypedBruijn = lambdaDeepShrink (flatShrink `composeShrink` elimanateBruijn) `composeShrink`
                    deepShrink (shrinkVal `composeShrink` (maybeToList . eval))

shrinkUnTypedBruijn :: LamTerm () Bound -> [LamTerm () Bound]
shrinkUnTypedBruijn = deepShrink (const [double 2] `composeShrink`
                                  flatShrink `composeShrink`
                                  shrinkVal `composeShrink`
                                  elimanateBruijn)
shrinkUnTypedLambda :: LamTerm () Name -> [LamTerm () Name]
shrinkUnTypedLambda  = deepShrink (const [double 2] `composeShrink`
                                   flatShrink `composeShrink`
                                   shrinkVal `composeShrink`
                                   elimanateLambda)

shrinkVal :: LamTerm () n -> [LamTerm () n]
shrinkVal (Val _ v) = val <$> shrinkValue v
shrinkVal _= []

flatShrink :: LamTerm () n -> [LamTerm () n]
flatShrink (Appl _ t1 t2) = [t1, t2]
flatShrink _ = []

lambdaDeepShrink :: (LamTerm () n -> [LamTerm () n]) -> LamTerm () n -> [LamTerm () n]
lambdaDeepShrink shrinker term = shrinker term ++ lambdaDeepShrink' term
    where lambdaDeepShrink' (Lambda () n t) = Lambda () n <$> lambdaDeepShrink shrinker t
          lambdaDeepShrink' _ = []



deepShrink :: (LamTerm () n -> [LamTerm () n]) -> LamTerm () n -> [LamTerm () n]
deepShrink shrinker term = shrinker  term ++ deepShrink' term
  where
    deepShrink' (Appl () t1 t2) =
                [Appl () t1' t2 | t1' <- deepShrink shrinker t1 ] ++
                [Appl () t1 t2' | t2' <- deepShrink shrinker t2 ]
    deepShrink' (Lambda () n t ) = Lambda () n <$> deepShrink shrinker t
    deepShrink' _ = []

composeShrink :: (a -> [a]) -> (a->[a]) -> a -> [a]
composeShrink f g a =  f a ++ g a

elimanateBruijn ::  BruijnTerm () -> [BruijnTerm ()]
elimanateBruijn (Lambda () _ term) =  go 0 term
  where
    go i1 (Var () (Bound i2))
      | i1 == i2 = mzero
      | i1 < i2 = return $ Var () $ Bound (i2 - 1)
      | otherwise = return $ Var () $ Bound i2
    go _ (v@Val {}) = return v
    go i (Lambda _ n t) = Lambda () n <$> go (i + 1) t
    go i (Appl _ t1 t2) = Appl () <$> go i t1 <*> go i t2
elimanateBruijn _ = []

elimanateLambda ::  LamTerm () Name -> [LamTerm () Name]
elimanateLambda (Lambda () name term) = if go term then [term] else []
  where
    go (Var () n)
      | n == name =False
      | otherwise =True
    go Val {} = True
    go (Lambda _ n t2)
      | n == name = True
      | otherwise = go t2
    go (Appl _ t1 t2) = go t1 || go t2
elimanateLambda _ = []

genTyped :: ArbiRef n => Gen (LamTerm () n )
genTyped = fromJust <$> genTerm (Just (TVar (Free (-1))))

genUnTyped :: ArbiRef n => Gen (LamTerm () n )
genUnTyped = fromJust <$> genTerm Nothing

genWithType :: ArbiRef n => Type -> Gen (Maybe (LamTerm () n ))
genWithType t = genTerm (Just t)

genTerm :: ArbiRef n => Maybe Type -> Gen ( Maybe (LamTerm () n ))
genTerm t = sized $ \ n -> runGenerartor $ arbitraryTerm n t [] defualtGenState

arbitraryTerm :: ArbiRef n => Int -> Maybe Type -> [Type] ->
      GenState n -> Generater (LamTerm () n)
arbitraryTerm n mabeytype maxlist s
  | n <= 1 = oneOfLogic [ arbitraryValue mabeytype
                        , arbitraryVar mabeytype s
                        ]
    -- shorter and parmiterzerd size
  | otherwise = do
      b <- case mabeytype of
        Just t -> do
          b1 <- typeSizeBigger 7 t
          b2 <- or <$> mapM (typeSizeBigger 7 ) maxlist
          return $! b1 || b2
        Nothing -> return False
      if b
      then mzero
      else whenBacksteps (< 20) (
           oneOfLogic [ arbitraryAppl n mabeytype maxlist s
                   , arbitraryLambda n mabeytype maxlist s
                   ] ) $ error $ show mabeytype ++ "\n" ++ show n

-- TODO fix also genarate var Empty
arbitraryVar :: ArbiRef n => Maybe Type -> GenState n -> Generater (LamTerm () n)
arbitraryVar t s = do
  (n, f) <- refFromState s
  unifyGen t (TVar f)
  return $ Var () n

arbitraryAppl :: ArbiRef n => Int -> Maybe Type -> [Type] ->
     GenState n -> Generater (LamTerm () n)
arbitraryAppl size mabeytype maxlist state = do
  sizeLeft <- chooseLogic (1, size - 1)
  let sizeRight = size - sizeLeft
  case mabeytype of
    Nothing -> do
      expr1 <- arbitraryTerm sizeLeft Nothing [] state
      expr2 <- arbitraryTerm sizeRight Nothing [] state
      return $ appl expr1 expr2
    Just t -> do
      newvar <- newFreeVar
      if sizeLeft < sizeRight
      then do
        expr1 <- arbitraryTerm sizeLeft (Just (TAppl (TVar newvar) t)) (TVar newvar : maxlist) state
        expr2 <- arbitraryTerm sizeRight (Just (TVar newvar)) maxlist state
        return $ appl expr1 expr2
      else do
        expr2 <- arbitraryTerm sizeRight (Just (TVar newvar)) (TAppl (TVar newvar) t : maxlist) state
        expr1 <- arbitraryTerm sizeLeft (Just (TAppl (TVar newvar) t)) maxlist state
        return $ appl expr1 expr2

arbitraryLambda :: ArbiRef n => Int -> Maybe Type -> [Type] ->
    GenState n -> Generater ( LamTerm () n)
arbitraryLambda size t maxlist state = do
  var1 <- newFreeVar
  var2 <- newFreeVar
  (n, newState) <- lift $ lift $ newVarRef state var1
  unifyGen t $ TAppl (TVar var1) (TVar var2)
  expr <- case t of
    Just _ -> arbitraryTerm (size - 1) (Just (TVar var2 )) maxlist newState
    Nothing -> arbitraryTerm (size - 1) Nothing [] newState
  return $ lambda n expr

newVarRef :: ArbiRef n => GenState n -> Free -> Gen (String, GenState n)
newVarRef state free = do
  let names = bToList $ tEnv state
  boolNewName <- case names of
      [] -> return True
      _ -> frequency [(4, return True), (1, return False)]
  newname <- if boolNewName
             then (: []) <$> choose ('a', 'z')
             else elements $ map (fst . snd) names
  return (newname, updateState state boolNewName newname free)

