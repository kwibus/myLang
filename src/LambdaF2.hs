{-#LANGUAGE MonoLocalBinds #-}
module LambdaF2 where

import Name
import Value
import Lambda as Lam
import Data.Bifunctor

data LamTermF i j n a = LambdaF [(i, Name)] a
            | ApplF a [a]
            | VarF j n
            | ValF j Value
            | LetF j [Def i a] a
            deriving (Eq, Show)

instance Functor (LamTermF i j n) where
    fmap f (LambdaF v t) = LambdaF v $ f t
    fmap f (ApplF t1 ts) = ApplF (f t1) (fmap f ts)
    fmap _ (VarF i n) = VarF i n
    fmap _ (ValF i v) = ValF i v
    fmap f (LetF i defs t ) = LetF i (map (fmap f) defs) $ f t

instance Traversable (LamTermF i j n) where
  traverse _ (VarF i b) = pure (VarF i b)
  traverse _ (ValF i v) = pure (ValF i v)
  traverse f (LambdaF v t) = LambdaF v <$> f t
  traverse f (ApplF t1 ts) = ApplF  <$> f t1 <*> traverse f ts
  traverse f (LetF i defs tn) = LetF i <$> traverse (traverse f ) defs <*> f tn

instance Foldable (LamTermF i j n) where
  foldr _ b VarF {} = b
  foldr _ b ValF {} = b
  foldr f b (LambdaF _ a) = f a b
  foldr f b (ApplF a1 as) = f a1 (foldr f b as)
  foldr f b (LetF _ defs an) = foldr (flip (foldr f) ) (f an b) defs

wrap :: LamTerm i j n -> LamTermF i j n (LamTerm i j n)
wrap (Lam.Var i n) = VarF i n
wrap (Lam.Val i v) = ValF i v
wrap t0@Lam.Lambda {} = LambdaF v t
  where
    (v, t) = go t0
    go (Lam.Lambda i n tn) = first ((i,n):) $ go tn
    go tn = ([],tn)
wrap t0@Lam.Appl {} = let (f:args)= accumulateArgs t0
                      in ApplF f args
wrap (Lam.Let i defs t) = LetF i defs t

mapLambdaM :: Monad m
           => (LamTerm i j n ->  LamTermF i j n (m a) -> m a)
           -> LamTerm i j n
           -> m a
mapLambdaM  f ast0 = go ast0
  where
    go ast = f ast ( go <$>  wrap ast)

foldLambdaM :: Monad m
            => (context -> LamTerm i j n ->  LamTermF i j n (context -> m a) -> m a)
            -> context
            -> LamTerm i j n
            -> m a
foldLambdaM f context0 ast0 = go ast0 context0
  where
    go ast context = case ast of
      Var i n -> f context ast (VarF i n)
      Val i v -> f context ast (ValF i v)
      _ -> f context ast $  fmap go (wrap ast)

bottumUpWithM :: Monad m
              => (context -> LamTerm i j n -> m context)
              -> (context -> LamTerm i j n -> LamTermF i j n a -> m a)
              -> context
              -> LamTerm i j n
              -> m a
bottumUpWithM updateContext f context0 ast0 = go context0 ast0
  where
   -- go :: context -> LamTerm i n -> m a
   go context ast = do
      newContext <- updateContext context ast
      astF <- traverse (go newContext) (wrap ast)
      f newContext ast astF

bottumUpWith :: (context -> LamTerm i j n -> context)
             -> (context -> LamTerm i j n -> LamTermF i j n a -> a)
             -> context
             -> LamTerm i j n
             -> a
bottumUpWith updateContext f context0 ast0 = go context0 ast0
  where
   -- go :: context -> LamTerm i n -> a
   go context ast = f newContext ast (go newContext <$> wrap ast)
    where
      newContext = updateContext context ast
