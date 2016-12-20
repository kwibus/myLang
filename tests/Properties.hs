module Properties where

import Data.List (foldl')
import Control.Monad
import Data.Maybe

import Lambda
import BruijnTerm
import BruijnEnvironment
import FreeEnvironment
import Type

normalised :: Eq i => BruijnTerm i -> Bool
normalised t = fmap lam2Bruijn (bruijn2Lam t) == return ( return t)

welFormd :: BruijnTerm i -> Bool
welFormd t0 = go t0 0
    where go (Lambda _ _ t) dept = go t (dept + 1)
          go (Appl t1 t2) dept = go t1 dept && go t2 dept
          go (Var _ (Bound i) ) dept = i < dept && i >= 0
          go Val {} _ = True
          go (Let _ defs term) dept = all (welFormdDef (dept + length defs)) defs  && go term (dept +length defs)
          welFormdDef i (Def _ _ t)  = go t i

-- TODO uneeded check
welFormdType :: Type -> Bool
welFormdType = go
    where go (TAppl t1 t2) = go t1 && go t2
          go (TVar (Free i) ) = i >= 0
          go (TPoly(Free i) ) = i >= 0
          go TVal {} = True

size :: LamTerm a i -> Int
size (Lambda _ _ e ) = size e + 1
size (Appl e1 e2) = size e1 + size e2 + 1
size (Let _ defs term) =sum (map sizeDefs  defs) + size term + 1
    where sizeDefs (Def _ _ t) = size t
size _ = 1

-- $setup
-- >>> import MakeTerm
--
-- |
-- isValueRestricted check for circular let defnition
-- isCircular (let a = a in a)
-- >>> isCirculair $ mkLet [("a",bvar 0)] $ bvar 0
-- True
--
-- isCircular (let a = b; b = a in b)
-- >>> isCirculair $ mkLet [("a",bvar 0),("b",bvar 1)] $ bvar 0
-- True
--
-- let a = f a in a
-- >>> isCirculair $ mkLet [("a",appl  (bvar 1)(bvar 0))] $ bvar 0
-- True
--
-- let a = let b = a in b in a
-- >>> isCirculair $ mkLet [("a",mkLet [("b",bvar 1 )] $ bvar 0)] $ bvar 0
-- True
--
-- let a = b ;b = b in  false
-- >>> isCirculair $ mkLet [("a",bvar 0 ),("b",bvar 0)] false
-- True
--
-- let f =\\a. g;  g =\\b.f in  g
-- >>> isCirculair $ mkLet [("f",lambda "a" $ bvar 0),("g",lambda "a" $ bvar 1)] $ bvar 0
-- False
--
-- let f = g true ;  g = \\a.f in  g
-- >>> isCirculair $ mkLet [("f",appl (bvar 0) true),("g",lambda "a" $ bvar 2)] $ bvar 0
-- True
--
-- let f = (\\a.true) fin  f
-- >>> isCirculair $ mkLet [("f",appl (lambda "a" true) (bvar 0))] $ bvar 0
-- True
--
-- let f = (\\a.f) true in  f
-- >>> isCirculair $ mkLet [("f",appl (lambda" a" (bvar 1)) true)] $ bvar 0
-- True
--
-- let f = \\x.y; y =y in y
-- >>> isCirculair $ mkLet [("f",lambda "x" (bvar 1)),("y",bvar 0)] $ bvar 0
-- True
--
-- let f = \\x y.a; a = true in false
-- >>> isCirculair $ mkLet [("f",lambda "x" $ lambda "y" (bvar 2)),("y",true)] $ false
-- False
--
-- let a = y;
--     f = \\x.false
--     y = let z = true
--         in f
-- in false
--
-- \\a.let b = c; c = True ; d = mkLet e = False in c in b
-- >>> isCirculair $ lambda "a" $ mkLet [("b", bvar 0),("c", true),("d", mkLet [("e", false)] (bvar 2))] (bvar 2)
-- False
--
-- let f = \x.y +
--     y = f
-- in f
-- >>> isCirculair $ mkLet [("x",lambda "j" $ appl (bvar 1) true),("y",bvar 1)] $ bvar 1
-- False

isCirculair :: Show i => BruijnTerm i -> Bool
isCirculair = go
  where
    go Val {} =  False
    go Var {} = False
    go (Lambda _ _ term) = go term
    go (Appl t1 t2) = go t1 || go t2
    go (Let _ defs term) = isCirculairLet defs  || go term

data TermState i = Unknow (BruijnTerm i)
                 | Forbidden
                 | Correct
                 deriving Show

-- TODO overcomplicated ?, premature optimalisation passing correct ?
isCirculairLet ::Show i => [Def i Bound] -> Bool
isCirculairLet = isNothing . checkDefs bEmtyEnv
  where
    -- checkDefs :: BruijnEnv (TermState i ) -> [Def i Bound] ->Maybe (BruijnEnv (TermState i))
    checkDefs env defs =
        let bounds = defsBounds defs
            newEnv = foldl' (\ envN (Def _ _ tn ) -> bInsert (Unknow tn) envN ) env defs
        in foldM checkDef newEnv bounds

    -- checkDef :: BruijnEnv (TermState i ) -> (Def i Bound,Bound) ->Maybe (BruijnEnv (TermState i))
    -- checkDef env b | traceShow (b,env)  False = undefined
    checkDef env b = case bMaybeLookup b env of
        (Just Correct) -> Just env
        (Just Forbidden) -> error "this cant happen"
        (Just (Unknow term@Lambda{})) ->
            checkTerm 0 (bReplace b Correct env) term
        (Just (Unknow term)) -> do
            env' <- checkTerm 0 (bReplace b Forbidden env) term
            return (bReplace b Correct env' )
        Nothing -> error "cant happen"
    -- checkTerm:: BruijnEnv (TermState i ) -> BruijnEnv i ->Maybe (BruijnEnv (TermState i))
    checkTerm depth env term = case term of
            Appl t1 t2 -> do
                env' <- checkTerm depth env t1
                checkTerm depth env' t2
            (Var _ (Bound b)) -> case  bMaybeLookup (Bound b) env of
                (Just Forbidden) ->  Nothing
                (Just Correct) -> Just env
                (Just Unknow {}) | b < depth -> Just env
                                 | otherwise -> let (initialEnv,dropOff) = bSplitAt depth env
                                               in bInserts dropOff <$> checkDef initialEnv (Bound $! b - depth)
                Nothing -> Just env
            Lambda _ _ t -> bDrop 1 <$> checkTerm  (depth+1) (bInsert Correct env) t
            Let _ defs' term' ->  bDrop (length defs') <$> do
                stat1 <- checkDefs env defs'
                checkTerm (depth + length defs') stat1 term'
            Val {} -> Just env
