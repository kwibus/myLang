
-- | This Module implementats eval by value for "BruijnTerm"
-- it's used in the "Interper.Main", but its more a prove of concept.
-- it purpose is:
--
-- * flesh out the denotatial semantics of the Language
--
-- * and testing how usefull MTable is.
--
-- eval could be made more efficient because it put some work in keeping free variables consistend
-- while it will crash if it applys a free variable

module Eval
where

import Data.Maybe

import Step hiding (map,last)
import qualified Step as S (map,last)

import Unprocessed
import BruijnTerm
import Value
import BruijnEnvironment
import MTable (empty)

import LambdaF
import qualified Data.DList as DList

-- |eval term in accordance with call by value.
-- If a term can't be further be evaluated it will return 'Nothing'
eval :: BruijnTerm () -> Maybe (BruijnTerm ())
eval = listToMaybe . evalSteps

-- | Big stap semantics for eval by value.
--
--  can diverg
fullEval :: BruijnTerm () -> BruijnTerm ()
fullEval = dToBruijn .fst . evalStepsW

-- | Small step semantics for eval
--
-- If evaluation is convergent the list is infinit
evalSteps :: BruijnTerm () -> [BruijnTerm ()]
evalSteps ast = snd $ evalStepsW  ast

evalStepsW :: BruijnTerm () -> (D,[BruijnTerm()])
evalStepsW term = runStep $ evalW bEmtyEnv $ Un empty term

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
--      it possibel to store Env in MTable,
--      no real reason to pass around 2 "env"
--          this opens the possibilty to store D and incFree instead of substitute
--          this removes the need for reEval defs
--          and might make mtable overkill
--
-- you might think that Reader monad for env mtable might be a good idea
-- this does not work well with incrementalM
-- and makes it easy to exedenaly peekM with not match MTable

evalW :: Env -> Unprocessed -> Step (BruijnTerm()) D
evalW env ast = case peek ast of
    (LambdaF _ n t) -> return $ Closure [] n t
    (VarF _ b) -> case maybeExtract b env of
        Just v -> yield (dToBruijn v) >> return v
        Nothing -> return $ Free b
    (ValF _ v) -> return $ DVal v
    (LetF _ oldDefs t) -> do
      (newEnv,newDefs) <- S.map (\defs -> Let () defs (proces t)) $ evalDefsW env oldDefs
      newT <- censors (Let () $ map (fmap d1ToBruijn) newDefs) $ evalW newEnv t
      shrink [newDefs] newT
    (ApplF t1 t2) -> do
      newT2 <- censors (Appl (proces t1)) $ evalW env t2
      newT1 <- censors (`Appl` dToBruijn newT2) $ evalW env t1
      reduce env newT1 newT2

reduce :: Env -> D -> D -> Step (BruijnTerm ()) D
reduce env d1 d2 = case d1 of
  (Closure  defs _ t) ->
    let  depthdiff = length $ concat defs
         newterm =  substitute depthdiff (dToBruijn d2)t
         -- a alternative method for getting newEnv is to store it in closure
         -- reduce should then no longer need env
         newEnv = foldr (store.map implementation) env defs
    in do newD <- censors (addPrefixLets defs) $ do
              yield (proces newterm)
              evalW newEnv newterm
          shrink defs newD
  (DVal v1) -> case d2 of
      (DVal v2) -> do
          let newV =  applyValue v1 v2
          yield (Val () newV)
          return $ DVal newV
      _ -> error $ "applied " ++ show v1 ++ " with " ++ show d2
  Free {} -> error "apply a free variable. This is not implemented"
  -- shrink (addDefs defs d)

shrink :: [[Def () D1]] -> D -> Step (BruijnTerm ()) D
shrink oldDefs (Closure newDefs name t) = return $ Closure (newDefs ++ oldDefs) name t
shrink def d  = go def
  where
    go [] = return d
    go (dropDefs:defs_) = do
        -- this incFreeD is ony over Dval/Free so should not be expesive
        yield $ addPrefixLets defs_ $ dToBruijn $ incFreeD (negate $ length dropDefs) d
        go  defs_

evalDefsW :: Env -> [Def () Unprocessed] ->  Step [Def () (BruijnTerm ())] (Env,[Def () D1] )
evalDefsW env defs = do
    let procesedDef = map (fmap proces) defs

        dumyEnv = store (map (convert . implementation) procesedDef) env

        convert :: BruijnTerm () -> D1
        convert (Lambda () name t) = Closure [] name t
        convert _ = error "depency on not yet evaluated defention"

    ((_,newEnv),defsS) <- incrementalM go (nDefs-1,dumyEnv) procesedDef
    return (newEnv,map (fmap dToD1) defsS)
  where
    nDefs = length defs
    go :: (Int,Env) -> Def () (BruijnTerm ()) -> Step (Def () (BruijnTerm ())) ((Int,Env),Def () D)
    go (n,envN) (Def () b t) = do
      v <- S.map (Def () b) $ evalW envN $ reproces t
      return ((n-1,update (Bound n) v envN),Def () b v)

-- |  this function executes incremental with the 'Step' over a list
-- it will preserver serounding contect of the elemts is is executing
-- so it will prepend already fully executed \"results\" and append not yet executed elemnts from the list
incrementalM :: (b -> a -> Step a (b, c)) -> b -> [a] -> Step [a] (b,[c])
incrementalM f b0 list = go b0 DList.empty list
  where
    -- go :: b -> DList.DList c -> [a] -> Step [a] (b,[c])
    go b cs [] = return (b,DList.toList cs)
    go b previousC (a:future) = do
        (maybeNewAs,(newB,newC)) <-S.last $ S.map (:future) $ f b a
        let resultA = maybe a head maybeNewAs
        S.map (resultA:) $ go newB  (DList.snoc previousC newC) future

-- | Denotation values or the values you get after evaluation
-- it makes sure you cant have 'Appl' or 'Let' result of eval
--
-- it is parameterised over ast to use in closure
data DenotationValue t = Closure [[ Def () D1]] Name t -- ^ coresponds toLambda/Function with all definitions it could refer to that are no longer in scope. The definitions are grouped the same as in let definitions. so the corresponding 'BruijnTerm' can be recreated. they are stored from new to old
                       | Free Bound
                       | DVal Value
  deriving (Show,Eq)

instance Functor DenotationValue where
  fmap f (Closure defs n t) = Closure defs n $ f t
  fmap _ (Free b) = Free b
  fmap _ (DVal v) = DVal v

--TODO fix names
type D = DenotationValue Unprocessed -- ^ short for Unprocessed DenotationValue
type D1 = DenotationValue (BruijnTerm ()) -- ^ short for DenotationValue BruijnTerm

dToBruijn ::  D -> BruijnTerm ()
dToBruijn = d1ToBruijn . dToD1

d1ToBruijn :: D1 -> BruijnTerm ()
d1ToBruijn (Closure defs name t) = addPrefixLets defs $ Lambda () name t
d1ToBruijn (Free b) = Var () b
d1ToBruijn (DVal v) = Val () v

-- | used to create 'BruijnTerm' from 'DenotationValue'
addPrefixLets :: [[Def () D1]] -> BruijnTerm () -> BruijnTerm ()
addPrefixLets defss t = foldl (\t_ defs_ -> Let () (map (fmap d1ToBruijn) defs_) t_) t defss

dToD1 :: D -> D1
dToD1 = fmap proces

incFreeD ::  Int -> D -> D
incFreeD  _ v@DVal {} = v
incFreeD inc (Free (Bound n)) = Free $ Bound $ n + inc
incFreeD inc (Closure defs name t) = Closure newDefs name newT
  where
    (depthDiff,newDefs) = incFreeOfsetDefs 0 inc defs
    newT = insertUndefined (depthDiff+1) $ Unprocessed.incFree inc t

incFreeD1 :: Int -> D1 -> D1
incFreeD1 = incFreeD1ofset 0

incFreeD1ofset :: Int -> Int -> D1 -> D1
incFreeD1ofset _ _ v@DVal {} = v
incFreeD1ofset _ inc (Free (Bound n)) = Free $ Bound $ n + inc
incFreeD1ofset ofset inc (Closure  defs name t) = Closure newDefs name $ incFreeOfset (ofset+depthDiff+1) inc t
 where
    (depthDiff,newDefs) = incFreeOfsetDefs ofset inc defs

incFreeOfsetDefs :: Int -> Int -> [[Def () D1]] -> (Int,[[Def () D1]])
incFreeOfsetDefs ofset inc defs = foldr
         (\def (depth,old) -> let newDepth = depth + length def:: Int
                              in (newDepth, map (fmap $ incFreeD1ofset (ofset+newDepth) inc) def : old))
         (0,[])
         defs
-- TODO is list best monoid for this ?
--
-- TODO  Int is used to calculated depth differnce
--       what is needed for inlining
--       this leaks the abstraction about Bruijnindexs
--       maybe always store depth defined
type Env = BruijnEnv (Int, D1)

store:: [D1] -> Env -> Env
store terms env = bInserts (zip (fromToZero (length terms - 1)) terms ) env

update :: Bound -> D -> Env -> Env
update b v env = bReplace b (ofset,dToD1 v) env
  where (ofset, _) =  bLookup b env

-- TODO add coment
maybeExtract ::  Bound -> Env -> Maybe D
maybeExtract b@(Bound n) env = do
    (ofset,v) <- bMaybeLookup b env
    Just $ incFreeD (n - ofset) $ fmap reproces v
    -- TODO this incfree is need because v change depth/level
    --      there are ways to avoid this (see SimpleEval)
    --      or we could delay/acumulate thile printing

extract :: Bound -> Env -> D
extract b env = fromMaybe  (error "variable not in Env") $ maybeExtract b env
