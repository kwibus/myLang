module ArbitraryLambda
where

import Data.List
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
import BruijnEnvironment
import FreeEnvironment
import Type
import MakeType
import TopologicalSort
import Name
import ArbiRef
import qualified ModificationTags as M

-- TODO dont why "again" is need here but otwersie test will stop after discard
-- TODO cleaner notation
-- TODO generrate noncircular direct
forAllNonCiculair :: Testable prop => (BruijnTerm () () -> prop) -> Property
forAllNonCiculair prop =again $ forAllTypedBruijn $ \ e -> case sortTerm e of
          Left {} -> discard
          (Right newT) -> prop $ M.applyModify newT

forAllTypedBruijn :: Testable prop => (BruijnTerm () () -> prop) -> Property
forAllTypedBruijn = forAllShowShrink genTyped {-BruijnTerm.pShow-}show shrinkTypedBruijn

forAllUnTypedLambda :: Testable prop => (LamTerm () () Name -> prop) -> Property
forAllUnTypedLambda = forAllShrink genUnTyped shrinkUntypedLambda

forAllUnTypedBruijn :: Testable prop => (BruijnTerm () () -> prop) -> Property
forAllUnTypedBruijn = forAllShrink genUnTyped shrinkUntypedBruijn

forAllShowShrink :: Testable prop => Gen a -> ( a -> String) -> (a -> [a]) -> (a -> prop) -> Property
forAllShowShrink gen myShow shrinker pf = MkProperty $
  gen >>= \ x ->
    unProperty $
    shrinking shrinker x $ \ x' ->
      counterexample (myShow x') (pf x')

shrinkTypedBruijn :: LamTerm () () Bound -> [LamTerm () () Bound]
shrinkTypedBruijn = lambdaDeepShrink (flatShrink `composeShrink` elimanateBruijn) `composeShrink`
                    deepShrink shrinkVal --TODO  also shrink with eval

shrinkUntypedBruijn :: LamTerm () () Bound -> [LamTerm () () Bound]
shrinkUntypedBruijn = deepShrink (const [double 2] `composeShrink`
                                  flatShrink `composeShrink`
                                  shrinkVal `composeShrink`
                                  elimanateBruijn)
shrinkUntypedLambda :: LamTerm () () Name -> [LamTerm () () Name]
shrinkUntypedLambda = deepShrink (const [double 2] `composeShrink`
                                   flatShrink `composeShrink`
                                   shrinkVal `composeShrink`
                                   elimanateLambda)

shrinkVal :: LamTerm () () n -> [LamTerm () () n]
shrinkVal (Val _ v) = val <$> shrinkValue v
shrinkVal _ = []

flatShrink :: LamTerm () () n -> [LamTerm () () n]
flatShrink (Appl t1 t2) = [t1, t2]
flatShrink _ = []

lambdaDeepShrink :: (LamTerm () () n -> [LamTerm () () n]) -> LamTerm () () n -> [LamTerm () () n]
lambdaDeepShrink shrinker term = shrinker term ++ lambdaDeepShrink' term
    where lambdaDeepShrink' (Lambda () n t) = Lambda () n <$> lambdaDeepShrink shrinker t
          lambdaDeepShrink' _ = []

deepShrink :: (LamTerm () () n -> [LamTerm () () n]) -> LamTerm () () n -> [LamTerm () () n]
deepShrink shrinker term = shrinker term ++ deepShrink' term
  where
    deepShrink' (Appl t1 t2) =
                [Appl t1' t2 | t1' <- deepShrink shrinker t1 ] ++
                [Appl t1 t2' | t2' <- deepShrink shrinker t2 ]
    deepShrink' (Lambda () n t ) = Lambda () n <$> deepShrink shrinker t
    deepShrink' _ = []

composeShrink :: (a -> [a]) -> (a -> [a]) -> a -> [a]
composeShrink f g a = f a ++ g a

--TODO add remove let def
elimanateBruijn :: BruijnTerm () () -> [BruijnTerm () () ]
elimanateBruijn (Lambda () _ term) = go 0 term
  where
    go i1 (Var () (Bound i2))
      | i1 == i2 = mzero
      | i1 < i2 = return $ Var () $ Bound (i2 - 1)
      | otherwise = return $ Var () $ Bound i2
    go _ (v@Val {}) = return v
    go i (Lambda _ n t) = Lambda () n <$> go (i + 1) t
    go i (Appl t1 t2) = Appl <$> go i t1 <*> go i t2
    go i (Let _ defs t) = Let () <$> mapM (elimanateDef (i + length defs)) defs <*> go (i + length defs) t
    elimanateDef i (Def _ n t) = Def () n <$> go i t
elimanateBruijn _ = []

--TODO add remove let def
elimanateLambda :: LamTerm () () Name -> [LamTerm () () Name]
elimanateLambda (Lambda () name term) = if go term then [term] else []
  where
    go (Var () n)
      | n == name = False
      | otherwise = True
    go Val {} = True
    go (Lambda _ n t2)
      | n == name = True
      | otherwise = go t2
    go (Appl t1 t2) = go t1 || go t2
    go (Let _ defs t) = not ( any (\ (Def _ n _) -> n == name) defs) || (all elimanatedDef defs && go t)
    elimanatedDef (Def _ _ t) = go t
elimanateLambda _ = []

genTyped :: ArbiRef n => Gen (LamTerm () () n )
genTyped = fromJust <$> genTerm (Just (tVar (-1)))

genUnTyped :: ArbiRef n => Gen (LamTerm () () n )
genUnTyped = fromJust <$> genTerm Nothing

genWithType :: ArbiRef n => Type -> Gen (Maybe (LamTerm () () n))
genWithType t = genTerm (Just t)

genTerm :: ArbiRef n => Maybe Type -> Gen ( Maybe (LamTerm () () n ))
genTerm t = sized $ \ n -> generateGen $ generateTerm n t

generateTerm :: ArbiRef n => Int -> Maybe Type -> Generater (LamTerm () () n)
generateTerm n t = arbitraryTerm n t [] defualtGenState

-- more qonsitent names
arbitraryTerm :: ArbiRef n => Int -> Maybe Type -> [Type] ->
      GenState n -> Generater (LamTerm () () n)
arbitraryTerm n mabeytype maxlist s
  | n <= 1 = oneOfLogic [ arbitraryValue mabeytype
                        , arbitraryVar mabeytype s
                        ]
    -- shorter and parmiterzerd size , rename abort
  | otherwise = do
      b <- case mabeytype of
        Just t -> do
          b1 <- typeSizeBigger 7 t
          b2 <- or <$> mapM (typeSizeBigger 7 ) maxlist
          return $! b1 || b2
        Nothing -> return False
      if b
      then mzero
      else oneOfLogic [ arbitraryAppl n mabeytype maxlist s
                      , arbitraryLambda n mabeytype maxlist s
                      , arbitraryLet n mabeytype maxlist s
                      ]

-- TODO fix also genarate var Empty/Free
arbitraryVar :: ArbiRef n => Maybe Type -> GenState n -> Generater (LamTerm () () n)
arbitraryVar t s = do
  (n, f) <- refFromState s
  unifyGen t (TVar f ())
  return $ Var () n

arbitraryAppl :: ArbiRef n => Int -> Maybe Type -> [Type] ->
     GenState n -> Generater (LamTerm () () n)
arbitraryAppl size mabeytype maxlist state = do
  sizeLeft <- chooseLogic (1, size - 2)
  let sizeRight = size - sizeLeft - 1
  case mabeytype of
    Nothing -> do
      expr1 <- arbitraryTerm sizeLeft Nothing [] state
      expr2 <- arbitraryTerm sizeRight Nothing [] state
      return $ appl expr1 expr2
    Just t -> do
      newvar <- newFreeVar
      if sizeLeft < sizeRight
      then do
        expr1 <- arbitraryTerm sizeLeft (Just (TVar newvar () ~> t)) (TVar newvar (): maxlist) state
        expr2 <- arbitraryTerm sizeRight (Just (TVar newvar ())) maxlist state
        return $ appl expr1 expr2
      else do
        expr2 <- arbitraryTerm sizeRight (Just (TVar newvar ())) (TVar newvar () ~> t : maxlist) state
        expr1 <- arbitraryTerm sizeLeft (Just (TVar newvar () ~> t)) maxlist state
        return $ appl expr1 expr2

arbitraryLambda :: ArbiRef n => Int -> Maybe Type -> [Type] ->
    GenState n -> Generater ( LamTerm () () n)
arbitraryLambda size t maxlist state = do
  var1 <- newFreeVar
  var2 <- newFreeVar
  (n, newState) <- lift $ lift $ newVarRef state var1
  unifyGen t $ TAppl (TVar var1 ()) (TVar var2 ())
  expr <- case t of
    Just _ -> arbitraryTerm (size - 1) (Just (TVar var2 ())) maxlist newState --TODO remove reption
    Nothing -> arbitraryTerm (size - 1) Nothing [] newState
  return $ lambda n expr

arbitraryLet :: ArbiRef n => Int -> Maybe Type -> [Type] -> GenState n -> Generater (LamTerm () () n)
arbitraryLet size t maxlist state =
    let minmalSize = 1::Int
        maxDefs = 5 :: Int
        maxnumberDefs = min ((size - 1) `div` minmalSize) (maxDefs + 1)
    in do
    numberDefs <- chooseLogic (1, maxnumberDefs)
    if numberDefs <= 1
    then mzero
    else do
            let totallExtra = (size - 1) - minmalSize * numberDefs
            randomextra <- lift $ lift $ uniformBucket numberDefs totallExtra -- this will not backtrack
            let (resultSize : varSize) = map (minmalSize +) randomextra
            vars <- replicateM (numberDefs - 1) newFreeVar
            (varNames, newState) <- lift $ lift $ makeVars state vars
            let newmaxlist = maxlist ++ map (\f -> TVar f ()) vars
            let (mkSmalDefsArgs,mkBigDefsArgs) = partition (\(_,_,_,defSize) -> defSize< resultSize) $ zip4 (defsBounds vars) vars (map Name varNames) $ sort varSize
            let mkDefs (b,v,name,sizeTerm) = do
                    -- TODO remove self from maxlist
                    -- TODO dont make self refrence values
                    termN <- arbitraryTerm sizeTerm (t >> Just (TVar v ())) maxlist (disableFromEnv b newState)
                    return $ Def () name termN
            smalDefs <- mapM mkDefs mkSmalDefsArgs
            term <- arbitraryTerm resultSize t newmaxlist newState
            bigDefs <- mapM mkDefs mkBigDefsArgs
           -- TODO maybe shuffle  but expensive  with BruijnTerm and would it make a difference
            return $ Let () (smalDefs++ bigDefs) term

--TODO should be a fold
makeVars :: ArbiRef n => GenState n -> [Free] -> Gen ([String], GenState n)
makeVars state [] = return ([], state)
makeVars state (f : fs) = do
    (newVar, newState) <- newVarRef state f
    (resetVar , finalState) <- makeVars newState fs
    return (newVar : resetVar, finalState)

-- TODO explain
uniformBucket :: Int -> Int -> Gen [Int]
uniformBucket 0 _ = return []
uniformBucket buckets totaal = do
    randomList <- replicateM (buckets - 1) $ choose (0, totaal) :: Gen [Int]
    return $ diff $ 0 : (sort randomList ++ [totaal])
    where diff :: [Int] -> [Int]
          diff (a : b : rest) = (b - a) : diff (b : rest)
          diff [_] = []
          diff [] = []

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
