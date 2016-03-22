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
import TypeCheck (apply)

forAllTypedBruijn :: Testable prop => (BruijnTerm () -> prop) -> Property
forAllTypedBruijn = forAllShowShrink genTyped printBrujin shrinkTyped

printBrujin :: BruijnTerm i -> String
printBrujin = either show PrettyPrint.pShow . bruijn2Lam . removeInfo

forAllUnTypedLambda :: Testable prop => (LamTerm () Name -> prop) -> Property
forAllUnTypedLambda = forAllShrink genUnTyped shrinkUntypedLamba

forAllUnTypedBruijn :: Testable prop => (BruijnTerm () -> prop) -> Property
forAllUnTypedBruijn = forAllShrink genUnTyped shrinkUntypedBruijn

forAllShowShrink :: Testable prop => Gen a -> ( a -> String) -> (a -> [a]) -> (a -> prop) -> Property
forAllShowShrink gen myShow shrinker pf =
  MkProperty $
  gen >>= \x ->
    unProperty $
    shrinking shrinker x $ \x' ->
      counterexample (myShow x') (pf x')

shrinkTyped :: BruijnTerm () -> [BruijnTerm () ]
shrinkTyped = shrinkTerm False elimanateBruijn

shrinkUntypedLamba :: LamTerm () Name -> [LamTerm () Name]
shrinkUntypedLamba = shrinkTerm True elimanateLambda

shrinkUntypedBruijn :: BruijnTerm () -> [BruijnTerm ()]
shrinkUntypedBruijn = shrinkTerm True elimanateBruijn

shrinkTerm :: Bool -> (Name -> LamTerm () n -> Maybe (LamTerm () n )) -> LamTerm () n -> [ LamTerm () n ]
shrinkTerm untyped elimanate = fastShrink True
    where fastShrink _ (Val _ v) = val <$> shrinkValue v
          fastShrink b t = whenTrue b [double 2.0] ++ shrinkT b t
          shrinkT b (Appl _ t1 t2) = whenTrue b [t1, t2] ++
                [Appl () t1' t2 | t1' <- fastShrink untyped t1 ] ++
                [Appl () t1 t2' | t2' <- fastShrink untyped t2 ]
          shrinkT b (Lambda _ name t) =
                    whenTrue b (maybeToList (elimanate name t))
                 ++ (lambda (toString name) <$> fastShrink untyped t)
          shrinkT _ (Val _ v) = val <$> shrinkValue v
          shrinkT _ _ = []

whenTrue :: Monoid a => Bool -> a -> a
whenTrue True a = a
whenTrue False _ = mempty

elimanateBruijn :: Name -> BruijnTerm () -> Maybe (BruijnTerm ())
elimanateBruijn _ = go 0
  where
    go i1 (Var () (Bound i2))
      | i1 == i2 = Nothing
      | i1 < i2 = Just $ Var () $ Bound (i2 - 1)
      | otherwise = Just $ Var () $ Bound i2
    go _ (v@Val {}) = Just v
    go i (Lambda _ n t) = Lambda () n <$> go (i + 1) t
    go i (Appl _ t1 t2) = Appl () <$> go i t1 <*> go i t2

elimanateLambda :: Name -> LamTerm () Name -> Maybe ( LamTerm () Name )
elimanateLambda name = go
  where
    go t@(Var () n)
      | n == name = Nothing
      | otherwise = Just t
    go v@Val {} = Just v
    go t1@(Lambda _ n t2)
      | n == name = Just t1
      | otherwise = Lambda () n <$> go t2
    go (Appl _ t1 t2) = Appl () <$> go t1 <*> go t2

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
      else whenBacksteps (< 20000) (
           oneOfLogic [ arbitraryAppl n mabeytype maxlist s
                   , arbitraryLambda n mabeytype maxlist s
                   ]
            ) $ do
                    sub <- getSub
                    return $ error $ show (apply sub <$> mabeytype) ++ "\n" ++ show n

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

