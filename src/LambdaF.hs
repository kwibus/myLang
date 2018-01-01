{-#LANGUAGE MonoLocalBinds #-}
module LambdaF where

import Name
import Value
import Lambda as Lam

-- TODO i is always () maybe remove

data LamTermF i n a = LambdaF i Name a
            | ApplF a a
            | VarF i n
            | ValF i Value
            | LetF i [Def i a] a
            deriving (Eq, Show)

instance Functor (LamTermF i n) where
    fmap f (LambdaF i n t) = LambdaF i n $ f t
    fmap f (ApplF t1 t2) = ApplF (f t1) (f t2)
    fmap _ (VarF i n) = VarF i n
    fmap _ (ValF i v) = ValF i v
    fmap f (LetF i defs t ) = LetF i (map (fmap f) defs) $ f t

instance Traversable (LamTermF i n) where
  traverse _ (VarF i b) = pure (VarF i b)
  traverse _ (ValF i v) = pure (ValF i v)
  traverse f (LambdaF i n t) = LambdaF i n <$> f t
  traverse f (ApplF t1 t2) = ApplF  <$> f t1 <*> f t2
  traverse f (LetF i defs tn) = LetF i <$> traverse (traverse f ) defs <*> f tn

instance Foldable (LamTermF i n) where
  foldr _ b VarF {} = b
  foldr _ b ValF {} = b
  foldr f b (LambdaF _ _ a) = f a b
  foldr f b (ApplF a1 a2) = f a1 (f a2 b )
  foldr f b (LetF _ defs an) = foldr (flip (foldr f) ) (f an b) defs

-- fold :: (b -> LamTermF i n a-> (a,b)) -> b -> a -> LamTerm i n
--FIXME rename
unfold :: (b -> a -> (LamTermF i n a,b)) -> b -> a -> LamTerm i n
unfold f b a = case f b a of
    (VarF i n,_) -> Lam.Var i n
    (ValF i v,_) -> Lam.Val i v
    (LambdaF i n t1, b1) -> Lam.Lambda i n (unfold f b1 t1)
    (ApplF t1 t2, b1)  -> Lam.Appl (unfold f b1 t1) (unfold f b1 t2)
    (LetF i defs t, b1) -> Lam.Let i (map (fmap $ unfold f b1) defs) (unfold f b1 t)

--FIXME rename
unfoldM :: Monad m => (b -> a -> m (LamTermF i n a,b)) -> b -> a -> m (LamTerm i n)
unfoldM f b a = do
  result <- f b a
  case result of
    (VarF i n,_) -> return $ Lam.Var i n
    (ValF i v,_) -> return $ Lam.Val i v
    (LambdaF i n t1, b1) -> Lam.Lambda i n <$> unfoldM f b1 t1
    (ApplF t1 t2, b1) -> Lam.Appl <$> unfoldM f b1 t1 <*> unfoldM f b1 t2
    (LetF i defs t, b1) -> Lam.Let i <$> mapM (mapM$ unfoldM f b1)defs <*> unfoldM f b1 t

wrap :: LamTerm i n -> LamTermF i n (LamTerm i n)
wrap (Lam.Var i n) = VarF i n
wrap (Lam.Val i v) = ValF i v
wrap (Lam.Lambda i n t) = LambdaF i n t
wrap (Lam.Appl t1 t2) = ApplF t1 t2
wrap (Lam.Let i defs t) = LetF i defs t

bottumUpWithM :: Monad m
              => (context -> LamTerm i n -> m context)
              -> (context -> LamTerm i n -> LamTermF i n a -> m a) -- TODO maybe change to LamTerm i n (m a) so you can change orer
              -> context
              -> LamTerm i n
              -> m a
bottumUpWithM updateContext f context0 ast0 = go context0 ast0
  where
   -- go :: context -> LamTerm i n -> m a
   go context ast = do
      newContext <- updateContext context ast
      astF <- traverse (go newContext) (wrap ast)
      f newContext ast astF

bottumUpWith :: (context -> LamTerm i  n -> context)
             -> (context -> LamTerm i n -> LamTermF i n a -> a)
             -> context
             -> LamTerm i n
             -> a
bottumUpWith updateContext f context0 ast0 = go context0 ast0
  where
   -- go :: context -> LamTerm i n -> a
   go context ast = f newContext ast (go newContext <$> wrap ast)
    where
      newContext = updateContext context ast
