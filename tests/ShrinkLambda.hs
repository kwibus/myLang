module ShrinkLambda
  ( shrinkTypedBruijn
  , shrinkUntypedBruijn
  , shrinkUntypedLambda
  )
where

import Control.Monad

import BruijnTerm
import MakeTerm

import ArbitraryValue (shrinkValue)

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


