module ArbitraryLambda
  ( forAllNonCiculair
  , forAllTypedBruijn
  , forAllUnTypedBruijn
  , forAllUnTypedLambda
  , genTyped
  , genWithType
  , genUnTyped
  , genTerm
  , shrinkTypedBruijn
  , shrinkUntypedBruijn
  , uniformBucket
  , defaultConf
  )
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
-- TODO generrate noncircular direct
forAllNonCiculair :: Testable prop => (BruijnTerm () () -> prop) -> Property
forAllNonCiculair prop =again $ forAllShowShrink (genTyped defaultConf{limitCirculair = True}) show shrinkTypedBruijn $ \ e -> case sortTerm e of
          Left {} -> discard
          (Right newT) -> prop $ M.applyModify newT

forAllTypedBruijn :: Testable prop => (BruijnTerm () () -> prop) -> Property
forAllTypedBruijn = forAllShowShrink (genTyped defaultConf) {-BruijnTerm.pShow-}show shrinkTypedBruijn

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

genTyped :: ArbiRef n => Conf -> Gen (LamTerm () () n )
genTyped conf = fromJust <$> genTerm conf (Just (tVar (-1)))

genUnTyped :: ArbiRef n => Gen (LamTerm () () n )
genUnTyped = fromJust <$> genTerm defaultConf Nothing

genWithType :: ArbiRef n => Type -> Gen (Maybe (LamTerm () () n))
genWithType t = genTerm defaultConf (Just t)

data Conf = Conf
    { maxTSize :: Int
    , maxNumDefs :: Int
    , limitCirculair:: Bool
    }

defaultConf :: Conf
defaultConf = Conf
  { maxTSize = 7
  , maxNumDefs = 5
  , limitCirculair = False
  }

--  TODO  stupid names
genTerm :: ArbiRef n => Conf -> Maybe Type -> Gen ( Maybe (LamTerm () () n ))
genTerm conf t = sized $ \ n -> generateGen $ generateTerm conf n t

generateTerm :: ArbiRef n => Conf -> Int -> Maybe Type -> Generater (LamTerm () () n)
generateTerm conf n t = arbitraryTerm conf n t [] defualtGenState

-- TODO more qonsitent names
-- arbitraryTerm should be arbitraryValue1 + arbitraryFunction
arbitraryTerm :: ArbiRef n => Conf -> Int -> Maybe Type -> [Type] ->
      GenState n -> Generater (LamTerm () () n)
arbitraryTerm conf n maybeType maxlist s
  | n <= 1 = oneOfLogic [ arbitraryValue maybeType
                        , arbitraryVar maybeType s
                        ]
  | otherwise = do
      validateTypeSize conf maybeType maxlist
      oneOfLogic [ arbitraryAppl conf n maybeType maxlist s
                 , arbitraryLambda conf n maybeType maxlist s
                 , arbitraryLet conf n maybeType maxlist s
                 ]

arbitraryFunction :: ArbiRef n => Conf -> Int -> Maybe Type -> [Type] ->
      GenState n -> Generater (LamTerm () () n)
arbitraryFunction conf n maybeType maxlist s
  | n <= 1 = mzero
  | otherwise  = do
      validateTypeSize conf maybeType maxlist
      -- TODO consider Lef [defs] function. see TopologicalSort
      arbitraryLambda conf n maybeType maxlist s

-- TODO rename arbitaryValue and rename arbitaryValue to arbitaryLit
arbitraryValue1 :: ArbiRef n => Conf -> Int -> Maybe Type -> [Type] ->
      GenState n -> Generater (LamTerm () () n)
arbitraryValue1 conf n maybeType maxlist s
  | n <= 1 = oneOfLogic [ arbitraryValue maybeType
                        , arbitraryVar maybeType s
                        ]
  | otherwise = do
      validateTypeSize conf maybeType maxlist
      oneOfLogic [ arbitraryAppl conf n maybeType maxlist s
                 , arbitraryLet conf n maybeType maxlist s
                 ]
validateTypeSize :: Conf -> Maybe Type -> [Type]-> Generater ()
validateTypeSize _      Nothing _ = return ()
validateTypeSize conf (Just  typeGoal) maxlist = do
          let check t = do
                  size <- typeSizeM t
                  guard (size < maxTSize conf)
          check typeGoal
          mapM_ check maxlist


-- TODO fix also genarate var Empty/Free
arbitraryVar :: ArbiRef n => Maybe Type -> GenState n -> Generater (LamTerm () () n)
arbitraryVar t s = do
  (n, f) <- refFromState s
  unifyGen t (TVar f ())
  return $ Var () n

arbitraryAppl :: ArbiRef n => Conf -> Int -> Maybe Type -> [Type] ->
     GenState n -> Generater (LamTerm () () n)
arbitraryAppl conf size mabeytype maxlist state = do
  sizeLeft <- chooseLogic (1, size - 2)
  let sizeRight = size - sizeLeft - 1
  case mabeytype of
    Nothing -> do
      expr1 <- arbitraryTerm conf sizeLeft Nothing [] state
      expr2 <- arbitraryTerm conf sizeRight Nothing [] state
      return $ appl expr1 expr2
    Just t -> do
      newvar <- newFreeVar
      if sizeLeft < sizeRight
      then do
        expr1 <- arbitraryTerm conf sizeLeft (Just (TVar newvar () ~> t)) (TVar newvar (): maxlist) state
        expr2 <- arbitraryTerm conf sizeRight (Just (TVar newvar ())) maxlist state
        return $ appl expr1 expr2
      else do
        expr2 <- arbitraryTerm conf sizeRight (Just (TVar newvar ())) (TVar newvar () ~> t : maxlist) state
        expr1 <- arbitraryTerm conf sizeLeft (Just (TVar newvar () ~> t)) maxlist state
        return $ appl expr1 expr2

arbitraryLambda :: ArbiRef n => Conf -> Int -> Maybe Type -> [Type] ->
    GenState n -> Generater ( LamTerm () () n)
arbitraryLambda conf size t maxlist state = do
  var1 <- newFreeVar
  var2 <- newFreeVar
  (n, newState) <- lift $ lift $ newVarRef state var1
  unifyGen t $ TAppl (TVar var1 ()) (TVar var2 ())
  expr <- case t of
    Just _ -> arbitraryTerm conf (size - 1) (Just (TVar var2 ())) maxlist newState --TODO remove reption
    Nothing -> arbitraryTerm conf (size - 1) Nothing [] newState
  return $ lambda n expr

arbitraryLet :: ArbiRef n => Conf -> Int -> Maybe Type -> [Type] -> GenState n -> Generater (LamTerm () () n)
arbitraryLet conf size t maxlist state =
    let maxNumExpresion = min (size - 1) (maxNumDefs conf + 1)
    in do
    numExpresion  <- chooseLogic (1, maxNumExpresion)
    if numExpresion <= 1
    then mzero
    else do
            -- pick sizes
            let totallExtra = (size - 1) -  numExpresion
            randomextra <- lift $ lift $ uniformBucket numExpresion totallExtra -- this will not backtrack
            let (resultSize : varSize) = map (1 +) randomextra

            -- add type for defs in env
            vars <- replicateM (numExpresion- 1) newFreeVar
            (varNames, newState) <- lift $ lift $ makeVars state vars -- Does not need to backtrack only name is random

            let newmaxs = map (\f -> TVar f ()) vars

            -- tuple for mkDefs are sorted on size on size, So terms can be calcluated from small to big, to prevent extra work
            let (mkSmalDefsArgs,mkBigDefsArgs) = partition (\(_,_,_,_,defSize) -> defSize< resultSize) $ zip5 (defsBounds vars) vars (map Name varNames) (unwrap newmaxs) (sort varSize)

            -- TODO maybe separate function
            let mkDefs (b,v,name,newMaxs,sizeTerm ) = do
                    let newMaxList = newMaxs ++ maxlist
                    termN <- if limitCirculair conf
                      -- TODO this limits some circulair definitions, but can still refere to self indirect via env and a other def
                      --      to prvent this you should add 5 empty to env,(does this work) and add real one to env after definition is generated
                      --      if you want the same distrobution you have to exclude function from this proces
                      then oneOfLogic
                        [ arbitraryValue1 conf sizeTerm (t >> Just (TVar v ())) newMaxList (disableFromEnv b newState) --TODO use the correct frequency
                        , arbitraryFunction conf sizeTerm (t >> Just (TVar v ())) newMaxList newState
                        ]
                      else arbitraryTerm conf sizeTerm (t >> Just (TVar v ())) newMaxList newState
                    return $ Def () name termN

            smalDefs <- mapM mkDefs mkSmalDefsArgs
            term <- arbitraryTerm conf resultSize t (newmaxs ++ maxlist) newState
            bigDefs <- mapM mkDefs mkBigDefsArgs
           -- TODO maybe shuffle  but expensive  with BruijnTerm and would it make a difference
            return $ Let () (smalDefs++ bigDefs) term

-- TODO better name
-- TODO droping original might be confusing
unwrap :: [a]   -> [[a]]
unwrap [] =  []
unwrap (_:as)  = as : unwrap as

--TODO should be a fold
makeVars :: ArbiRef n => GenState n -> [Free] -> Gen ([String], GenState n)
-- makeVars state0 fs = foldM (\(prevNames,state) f -> do
--                         (name, newstate ) <- newVarRef state f
--                         return (prevNames ++[name]  ,newstate))
--     ([],state0) fs
-- makeVars state0  fs = swap <$> mapAccumM (newVarRef1) state0 fs
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
