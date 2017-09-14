{-# LANGUAGE FlexibleContexts #-}
module Eval
where

import Control.Monad.Writer.Lazy
import Data.Maybe
import Data.Bifunctor

import Unprocessed
import BruijnTerm
import Value
import BruijnEnvironment
import MTable (empty)
import qualified TaggedLambda as Tag

import LambdaF
import qualified Data.DList as DList

type Step w a = Writer [w] a

--TODO fix names
data D = D [[ Def () D1]] Unprocessed
         deriving Show

data D1 = D1 [[ Def () D1]] (BruijnTerm ())
        deriving (Show,Eq)

-- |eval term in accordance with call by value.
-- If a term can't be further be evaluated it will return 'Nothing'
eval :: BruijnTerm () -> Maybe (BruijnTerm ())
eval = listToMaybe . evalSteps

fullEval :: BruijnTerm () -> BruijnTerm ()
fullEval = dToBruijn .fst . evalStepsW

dToBruijn ::  D -> BruijnTerm ()
dToBruijn (D defs t) = d1ToBruijn (D1 defs $proces t)

d1ToBruijn :: D1 -> BruijnTerm ()
d1ToBruijn (D1 defs t) = addPrefix defs t

addPrefix :: [[Def () D1]] -> BruijnTerm () -> BruijnTerm ()
addPrefix defss t = foldl (\t_ defs_ -> Let () (map (fmap d1ToBruijn) defs_) t_) t defss

dToD1 :: D -> D1
dToD1 (D defs t) = D1 defs (proces t)

evalSteps :: BruijnTerm () -> [BruijnTerm ()]
evalSteps ast = snd $ evalStepsW  ast

evalStepsW :: BruijnTerm () -> (D,[BruijnTerm()])
evalStepsW term = runWriter $ evalW bEmtyEnv $ Un empty $ Tag.tag term

retell :: (w1 -> w2) ->  Step w1 a -> Step w2 a
retell f = mapWriterT (fmap $ second (map f))

censors ::  MonadWriter [w] m => (w->w) -> m a -> m a
censors f = censor (map f)

-- TODO  Int is used to calculated depth differnce
--       what is needed for inlining
--       this leaks the abstraction about Bruijnindexs
--       maybe always store depth defined
type Env = BruijnEnv (Int, D1)

-- TODO add coment
maybeExtract ::  Bound -> Env -> Maybe D
maybeExtract b@(Bound n) env = do
    (ofset,D1 defs term) <- bMaybeLookup b env
    return $ D (map (map $ fmap $ incFreeD1 (n-ofset)) defs) $
               reproces $ incFreeOfset (length $ concat defs) (n-ofset) term

-- TODO write test for this
incFreeD1 :: Int -> D1 -> D1
incFreeD1 = incFreeD1ofset 0

incFreeD1ofset :: Int -> Int -> D1 -> D1
incFreeD1ofset ofset n (D1 defs t) = D1 newDefs$ incFreeOfset (ofset+dept) n t
  where
     (dept,newDefs) = foldr
         (\def (depth,old) -> let newDepth = depth+length def
                              in (newDepth, map (fmap $ incFreeD1ofset (ofset+newDepth) n) def : old))
         (0,[])
         defs

extract :: Bound -> Env -> D
extract b env = fromMaybe  (error "variable not in Env") $ maybeExtract b env

store:: [D1] -> Env -> Env
store terms env = bInserts (zip  (fromToZero (length terms - 1)) terms ) env

update :: Bound -> D -> Env -> Env
update b (D defs t) env = bReplace b (ofset,D1 defs $ proces t) env
  where (ofset, _) =  bLookup b env

-- TODO
--      it is possibel to chage, to make denotatial value more explict
--      which makes it more save
--      Data D = Closure [[Def () d]] Lambda
--             | Val v
--             | Free bound
--      eval :: .. -> D
--      reduce :: .. D -> D -> D
--      shrink [Dev d] -> D -> D
--
--      it possibel to store Env  in MTable,
--      no real reason to pass around 2 "env"
--          this opens the possibilty to store D and incFree instead of substitute
--          this removes the need for reEval defs
--          and might make mtable opsoute /simple
--
--      Unprocessed is now (Mtable , Taged Lambda)
--      but tages are only used top level
--      so you could change it to
--      Unprocessed  = (MTable,  BruijnTerm)
--      this could short circuit proces if there are no modifications

evalW :: Env -> Unprocessed -> Step (BruijnTerm()) D
evalW env ast = case peek ast of
    (LambdaF _ n t) -> return $ D [] ast
    (VarF _ b) -> case maybeExtract b env of
        Just v -> tell [dToBruijn v] >> return v
        Nothing -> return $ D []  ast
    (ValF _ v) -> return $ D [] $ val v
    (LetF _ oldDefs t) -> do
      (newEnv,newDefs) <- retell (\defs -> Let () defs (proces t)) $ evalDefsW env oldDefs
      newT <- censors (Let () $ map (fmap d1ToBruijn) newDefs) $ evalW newEnv t
      shrink $ addDefs [newDefs] newT
    (ApplF t1 t2) -> do
      newT2 <- censors (Appl (proces t1)) $ evalW env t2
      newT1@(D defs _) <- censors (`Appl` dToBruijn newT2) $ evalW env t1
      reduce (foldr (store.map implementation) env defs) newT1 newT2

addDefs :: [[Def () D1]] -> D -> D
addDefs oldDefs (D newDefs t) = D (newDefs ++ oldDefs) t

reduce :: Env -> D -> D -> Step (BruijnTerm ()) D
reduce env (D defs t1) newT2 = do
  d <- censors (addPrefix defs) $ case peek t1 of
    (LambdaF _ _ t) ->
      let newterm =  substitute depthdiff (dToBruijn newT2)t
          depthdiff = length $ concat defs
      in tell [proces newterm] >> evalW env newterm

    (ValF _ v1) -> case newT2  of
      (D [] t) -> case getVal t of
        (Just  v2) -> do
          let newTerm =  Val () $ applyValue v1 v2
          tell [newTerm]
          return $ D defs $ reproces newTerm
        Nothing -> error $ "applied " ++ show v1 ++ " with " ++ show newT2
      _ -> error "val can't be applied to lets" -- TODO
    _ -> error ( "reduced a non denotatial value:\n" ++ show ( proces t1))
  shrink (addDefs defs d)

shrink :: D -> Step (BruijnTerm ()) D
shrink d@(D [] _) = return d
shrink d@(D defs t) = case getVal t of
  (Just v) -> go defs
    where
        go [] = return ( D [] (val v))
        go (_:defs_) = tell [d1ToBruijn $ D1 defs_ $ Val () v] >> go  defs_
  Nothing -> return d

evalDefsW :: Env -> [Def () Unprocessed] ->  Step [Def () (BruijnTerm ())] (Env,[Def () D1 ] )
evalDefsW env defs = do
    -- TODO make dumy env only insert functions
    let procesedDef = map (fmap proces) defs
        dumyEnv = store (map (\(Def _ _ t) -> ( D1 []  t)) procesedDef) env
    ((_,newEnv),defsS) <- incrementalM go (nDefs-1,dumyEnv) procesedDef
    return (newEnv,map (fmap dToD1) defsS)
  where
    nDefs = length defs
    go :: (Int,Env) -> Def () (BruijnTerm ()) -> Step (Def () (BruijnTerm ())) ((Int,Env),Def () D)
    go (n,envN) (Def () b t) = do
      v <- retell (Def () b) $ evalW envN $ reproces t
      return ((n-1,update (Bound n) v envN),Def () b v)

incrementalM :: (b -> a -> Step a (b, c)) -> b -> [a] -> Step [a] (b,[c])
incrementalM f b0 list = go b0 DList.empty DList.empty list
  where
    go _ _ _ [] = error "incrementalM does work on empty list"
    go b previousA previousC [a] = do
      (newB,newC) <- retell (DList.apply previousA .return ) $ f b a
      return (newB,DList.apply previousC [newC])
    go b previousA previousC (a:future) =
      let ((newB,newC),newAs) = runWriter  $ f b a
      in do
        tell (map (\a'-> DList.apply previousA ( a':future)) newAs)
        go newB (DList.snoc previousA  (saveLast a newAs)) (DList.snoc previousC newC ) future

--TODO replace
saveLast :: a  -> [a] -> a
saveLast a as = last (a:as)
